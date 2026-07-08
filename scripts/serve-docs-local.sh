#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SITE_DIR="$ROOT/_site"
HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8099}"
BUILD_ONLY=0

usage() {
  cat <<'EOF'
Usage: scripts/serve-docs-local.sh [options]

Build and serve the Magent documentation site locally.

Options:
  --host HOST       Bind host. Default: 127.0.0.1
  --port PORT       Preferred port. Default: 8099
  --build-only      Build _site and exit without serving
  -h, --help        Show this help

Environment:
  HOST=0.0.0.0 PORT=4000 scripts/serve-docs-local.sh

The script exports Org documentation with Emacs and serves _site with Python's
built-in HTTP server.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host)
      HOST="${2:?missing value for --host}"
      shift 2
      ;;
    --port)
      PORT="${2:?missing value for --port}"
      shift 2
      ;;
    --build-only)
      BUILD_ONLY=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Missing required command: $1" >&2
    exit 1
  fi
}

choose_port() {
  require_cmd python3
  python3 - "$HOST" "$PORT" <<'PY'
import socket
import sys

host = sys.argv[1]
start = int(sys.argv[2])

for port in range(start, start + 100):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        sock.bind((host, port))
    except OSError:
        sock.close()
        continue
    sock.close()
    print(port)
    sys.exit(0)

raise SystemExit(f"no free port found from {start} to {start + 99}")
PY
}

serve_static_site() {
  require_cmd python3
  local actual_port
  actual_port="$(choose_port)"
  echo "Serving Magent docs at http://$HOST:$actual_port/"
  cd "$SITE_DIR"
  exec python3 -m http.server "$actual_port" --bind "$HOST"
}

"$ROOT/scripts/build-docs.sh"
if [[ "$BUILD_ONLY" -eq 0 ]]; then
  serve_static_site
fi
