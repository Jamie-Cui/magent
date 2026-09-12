"""Local-only chat stream: tool, truncated reasoning, then final answer.

Usage: python3 test/magent-stream-fixture.py PORT_FILE REPORT_FILE [MODE]
MODE is clean-eof (default), curl18-reasoning, or curl18-tool.
The report contains only counts and request equality, never request contents.
"""

# Copyright (C) 2026 Jamie Cui
# SPDX-License-Identifier: GPL-3.0-or-later

import argparse
import json
import time
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("port_file")
parser.add_argument("report_file")
parser.add_argument("mode", nargs="?", default="clean-eof",
                    choices=("clean-eof", "curl18-reasoning", "curl18-tool"))
args = parser.parse_args()


def chunk(delta, finish=None):
    return "data: " + json.dumps({
        "choices": [{"index": 0, "delta": delta, "finish_reason": finish}],
    }) + "\n\n"


class Fixture(BaseHTTPRequestHandler):
    requests = []

    def log_message(self, *_args):
        pass

    def do_POST(self):
        body = self.rfile.read(int(self.headers["Content-Length"]))
        self.requests.append(json.loads(body))
        number = len(self.requests)
        if number == 1:
            response = chunk({"content": "Checking fixture output."})
            response += chunk({"tool_calls": [{
                "index": 0, "id": "fixture_call", "type": "function",
                "function": {"name": "read_tool_output", "arguments": "{}"},
            }]}, "tool_calls") + "data: [DONE]\n\n"
        elif number == 2:
            response = chunk({"content": None, "reasoning_content": "Inspecting the result."})
            if args.mode == "curl18-tool":
                response += chunk({"tool_calls": [{
                    "index": 0, "id": "unfinished_call", "type": "function",
                    "function": {"name": "read_tool_output", "arguments": "{\"id\":"},
                }]})
        else:
            response = chunk({"content": "MAGENT_LOCAL_RETRY_OK"}, "stop") + "data: [DONE]\n\n"
        encoded = response.encode()
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        # Advertise more bytes than we send to reproduce real curl exit 18.
        missing_bytes = 100 if number == 2 and args.mode != "clean-eof" else 0
        self.send_header("Content-Length", str(len(encoded) + missing_bytes))
        self.end_headers()
        self.wfile.write(encoded)
        self.wfile.flush()


with HTTPServer(("127.0.0.1", 0), Fixture) as server:
    server.timeout = 1
    Path(args.port_file).write_text(str(server.server_port))
    print("Fixture ready on loopback", flush=True)
    deadline = time.monotonic() + 30
    while len(Fixture.requests) < 3 and time.monotonic() < deadline:
        server.handle_request()

requests = Fixture.requests
report = {
    "mode": args.mode,
    "requests": len(requests),
    "retry_input_identical": len(requests) == 3 and requests[1] == requests[2],
    "tool_results_in_retry": sum(
        message.get("role") == "tool" for message in requests[-1].get("messages", [])
    ) if requests else 0,
}
Path(args.report_file).write_text(json.dumps(report))
