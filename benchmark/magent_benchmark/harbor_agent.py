"""Harbor installed-agent adapter for Magent.

The task container remains Harbor's responsibility.  This adapter only installs
Emacs/Magent, calls the public Magent runtime, and converts its ledger to ATIF.
"""

from __future__ import annotations

from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path, PurePosixPath
import re
from typing import Any, override

from harbor.agents.installed.base import BaseInstalledAgent
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext
from harbor.models.trajectories import (
    Agent,
    FinalMetrics,
    Observation,
    ObservationResult,
    Step,
    ToolCall,
    Trajectory,
)
from harbor.models.trial.paths import EnvironmentPaths
from harbor.utils.trajectory_utils import format_trajectory_json


REMOTE_ROOT = PurePosixPath("/installed-agent/magent")
REMOTE_ELPA = PurePosixPath("/installed-agent/elpa")
REMOTE_EMACS = PurePosixPath("/installed-agent/emacs")
REMOTE_RUNNER = PurePosixPath("/installed-agent/magent-benchmark-runner.el")
REMOTE_INSTALLER = PurePosixPath("/installed-agent/magent-install-deps.el")


def _tree_digest(root: Path) -> str:
    digest = hashlib.sha256()
    for directory in (
        "lisp",
        "prompts",
        "prompt",
        "skills",
        "capabilities",
        "benchmark/elisp",
    ):
        path = root / directory
        if not path.is_dir():
            continue
        for file in sorted(
            p
            for p in path.rglob("*")
            if p.is_file() and p.suffix != ".elc" and "__pycache__" not in p.parts
        ):
            digest.update(str(file.relative_to(root)).encode())
            digest.update(b"\0")
            digest.update(file.read_bytes())
            digest.update(b"\0")
    adapter = root / "benchmark" / "magent_benchmark" / "harbor_agent.py"
    if adapter.is_file():
        digest.update(str(adapter.relative_to(root)).encode())
        digest.update(b"\0")
        digest.update(adapter.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def _iso(timestamp: Any) -> str | None:
    if timestamp is None:
        return None
    try:
        return datetime.fromtimestamp(float(timestamp), tz=timezone.utc).isoformat()
    except (TypeError, ValueError, OSError, OverflowError):
        return None


def _as_text(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, str):
        return value
    return json.dumps(value, ensure_ascii=False, sort_keys=True)


def _as_arguments(value: Any) -> dict[str, Any]:
    if isinstance(value, dict):
        return value
    if value is None:
        return {}
    return {"value": value}


def _usage_int(value: Any, *keys: str) -> int:
    if not isinstance(value, dict):
        return 0
    for key in keys:
        candidate = value.get(key)
        if isinstance(candidate, (int, float)) and not isinstance(candidate, bool):
            return int(candidate)
    return 0


def _usage_totals(result: dict[str, Any]) -> tuple[int, int, int]:
    input_tokens = output_tokens = cached_tokens = 0
    samples = result.get("usage-samples", result.get("usage_samples", []))
    if not isinstance(samples, list):
        return 0, 0, 0
    for sample in samples:
        input_tokens += _usage_int(
            sample, "input", "input_tokens", "prompt_tokens", "prompt"
        )
        output_tokens += _usage_int(
            sample, "output", "output_tokens", "completion_tokens", "completion"
        )
        cached_tokens += _usage_int(
            sample,
            "cached",
            "cached_tokens",
            "cache_read_input_tokens",
            "cache_read",
        )
    return input_tokens, output_tokens, cached_tokens


def ledger_to_trajectory(
    ledger: dict[str, Any],
    result: dict[str, Any],
    *,
    model_name: str | None,
    version: str,
    input_price_per_million: float | None = None,
    output_price_per_million: float | None = None,
    cached_input_price_per_million: float | None = None,
) -> Trajectory:
    """Convert a Magent materialized ledger and result to ATIF v1.7."""
    turns = ledger.get("turns") or []
    steps: list[Step] = []
    step_id = 1
    tool_count = 0

    for turn in turns:
        if not isinstance(turn, dict):
            continue
        items = turn.get("items") or []
        user_items = [
            item
            for item in items
            if isinstance(item, dict)
            and item.get("type") == "message"
            and item.get("role") == "user"
        ]
        if user_items:
            for item in user_items:
                steps.append(
                    Step(
                        step_id=step_id,
                        timestamp=_iso(item.get("created-at")),
                        source="user",
                        message=_as_text(item.get("content")),
                    )
                )
                step_id += 1
        elif isinstance(turn.get("input"), str):
            steps.append(
                Step(
                    step_id=step_id,
                    timestamp=_iso(turn.get("queued-at")),
                    source="user",
                    message=turn["input"],
                )
            )
            step_id += 1

        # The ledger merges each tool call/result into one lifecycle item.
        # Emit actions before the final assistant message even though the
        # streaming message item itself is allocated at the turn's start.
        for item in items:
            if not isinstance(item, dict) or item.get("type") != "tool":
                continue
            tool_count += 1
            call_id = str(item.get("call-id") or item.get("id") or f"tool-{tool_count}")
            output = item.get("output")
            if output is None:
                output = item.get("error")
            steps.append(
                Step(
                    step_id=step_id,
                    timestamp=_iso(item.get("created-at")),
                    source="agent",
                    message="",
                    model_name=model_name,
                    tool_calls=[
                        ToolCall(
                            tool_call_id=call_id,
                            function_name=str(item.get("name") or "unknown"),
                            arguments=_as_arguments(item.get("input")),
                        )
                    ],
                    observation=Observation(
                        results=[
                            ObservationResult(
                                source_call_id=call_id,
                                content=_as_text(output),
                                extra={
                                    "status": str(item.get("status") or "unknown"),
                                    "phase": item.get("phase"),
                                },
                            )
                        ]
                    ),
                    extra={"ledger_item_id": item.get("id")},
                )
            )
            step_id += 1

        assistant_items = [
            item
            for item in items
            if isinstance(item, dict)
            and item.get("type") == "message"
            and item.get("role") == "assistant"
        ]
        for item in assistant_items:
            steps.append(
                Step(
                    step_id=step_id,
                    timestamp=_iso(item.get("completed-at") or item.get("updated-at")),
                    source="agent",
                    message=_as_text(item.get("content")),
                    model_name=model_name,
                    llm_call_count=1,
                    extra={"ledger_item_id": item.get("id")},
                )
            )
            step_id += 1

    if not steps:
        # ATIF requires at least one step; result output preserves failure data.
        steps.append(
            Step(
                step_id=1,
                source="agent",
                message=_as_text(result.get("output") or result.get("error")),
                model_name=model_name,
                llm_call_count=1,
            )
        )

    input_tokens, output_tokens, cached_tokens = _usage_totals(result)
    cost: float | None = None
    if input_price_per_million is not None and output_price_per_million is not None:
        cached_price = (
            input_price_per_million
            if cached_input_price_per_million is None
            else cached_input_price_per_million
        )
        cost = (
            max(0, input_tokens - cached_tokens) * input_price_per_million
            + cached_tokens * cached_price
            + output_tokens * output_price_per_million
        ) / 1_000_000
    return Trajectory(
        schema_version="ATIF-v1.7",
        session_id=str(ledger.get("session-id") or ledger.get("id") or "unknown"),
        agent=Agent(
            name="magent",
            version=version,
            model_name=model_name,
            extra={
                "provider": result.get("provider"),
                "wire_api": result.get("wire-api", result.get("wire_api")),
            },
        ),
        steps=steps,
        final_metrics=FinalMetrics(
            total_prompt_tokens=input_tokens or None,
            total_completion_tokens=output_tokens or None,
            total_cached_tokens=cached_tokens or None,
            total_cost_usd=cost,
            total_steps=len(steps),
            extra={"tool_calls": tool_count},
        ),
        notes=(
            "Magent's ledger merges tool call/result lifecycles. Sampling token "
            "usage is captured by benchmark-only advice around normalized LLM events."
        ),
    )


class MagentHarborAgent(BaseInstalledAgent):
    """Run the repository's Magent source as a Harbor installed agent."""

    SUPPORTS_ATIF: bool = True
    SUPPORTS_RESUME: bool = False

    def __init__(
        self,
        *args: Any,
        provider: str,
        base_url: str,
        wire_api: str,
        api_key_env: str,
        reasoning_effort: str = "auto",
        source_root: str | None = None,
        elpa_bundle: str | None = None,
        emacs_bundle: str | None = None,
        input_price_per_million: float | None = None,
        output_price_per_million: float | None = None,
        cached_input_price_per_million: float | None = None,
        **kwargs: Any,
    ) -> None:
        root = Path(source_root).resolve() if source_root else Path(__file__).resolve().parents[2]
        if not (root / "lisp" / "magent.el").is_file():
            raise ValueError(
                f"Magent source root is invalid: {root}; run Harbor from benchmark/ "
                "or pass kwargs.source_root"
            )
        self._source_root = root
        self._benchmark_root = Path(__file__).resolve().parents[1]
        self._provider = provider
        self._base_url = base_url
        self._wire_api = wire_api
        self._api_key_env = api_key_env
        self._reasoning_effort = reasoning_effort
        self._elpa_bundle = Path(elpa_bundle).resolve() if elpa_bundle else None
        self._emacs_bundle = Path(emacs_bundle).resolve() if emacs_bundle else None
        self._input_price = input_price_per_million
        self._output_price = output_price_per_million
        self._cached_input_price = cached_input_price_per_million
        if wire_api not in {"responses", "chat_completions"}:
            raise ValueError(f"Unsupported Magent wire API: {wire_api}")
        kwargs.setdefault("version", f"source-{_tree_digest(root)[:12]}")
        super().__init__(*args, **kwargs)

    @staticmethod
    @override
    def name() -> str:
        return "magent"

    @override
    async def install(self, environment: BaseEnvironment) -> None:
        emacs_package = "" if self._emacs_bundle else "emacs-nox "
        apt_runtime = (
            "libgnutls30 libjansson4 libncurses6 libxml2 zlib1g "
            if self._emacs_bundle
            else ""
        )
        portable_runtime = (
            "gnutls jansson ncurses-libs libxml2 zlib "
            if self._emacs_bundle
            else ""
        )
        await self.exec_as_root(
            environment,
            command=(
                "if command -v apt-get >/dev/null 2>&1; then "
                "apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y "
                f"{emacs_package}{apt_runtime}ca-certificates curl git ripgrep; "
                "elif command -v apk >/dev/null 2>&1; then "
                f"apk add --no-cache {emacs_package}{portable_runtime}"
                "ca-certificates curl git ripgrep; "
                "elif command -v yum >/dev/null 2>&1; then "
                f"yum install -y {emacs_package}{portable_runtime}"
                "ca-certificates curl git ripgrep; "
                "else echo 'No supported package manager for Emacs' >&2; exit 1; fi"
            ),
        )
        if self._emacs_bundle:
            missing_bins = [
                name
                for name in ("emacs", "emacsclient")
                if not (self._emacs_bundle / "bin" / name).is_file()
            ]
            if missing_bins:
                raise ValueError(
                    f"Emacs bundle {self._emacs_bundle} is missing: "
                    + ", ".join(f"bin/{name}" for name in missing_bins)
                )
            await environment.upload_dir(self._emacs_bundle, str(REMOTE_EMACS))
            await self.exec_as_root(
                environment,
                command=(
                    f"ln -sf {REMOTE_EMACS / 'bin' / 'emacs'} /usr/local/bin/emacs; "
                    f"ln -sf {REMOTE_EMACS / 'bin' / 'emacsclient'} "
                    "/usr/local/bin/emacsclient"
                ),
            )
        await self.exec_as_agent(
            environment,
            command=(
                "actual=$(emacs -Q --batch --eval '(princ emacs-version)'); "
                "first=$(printf '%s\\n%s\\n' 29.1 \"$actual\" | sort -V | head -n1); "
                "if [ \"$first\" != 29.1 ]; then "
                "echo \"Magent requires Emacs >=29.1, found $actual\" >&2; exit 1; fi; "
                "mkdir -p /logs/agent; printf '%s\\n' \"$actual\" > /logs/agent/emacs-version.txt"
            ),
        )

        await environment.upload_dir(self._source_root / "lisp", str(REMOTE_ROOT / "lisp"))
        prompt_dir = self._source_root / "prompts"
        if not prompt_dir.is_dir():
            prompt_dir = self._source_root / "prompt"
        await environment.upload_dir(prompt_dir, str(REMOTE_ROOT / "prompts"))
        await environment.upload_dir(self._source_root / "skills", str(REMOTE_ROOT / "skills"))
        capabilities = self._source_root / "capabilities"
        if capabilities.is_dir():
            await environment.upload_dir(capabilities, str(REMOTE_ROOT / "capabilities"))
        await environment.upload_file(
            self._benchmark_root / "elisp" / "runner.el", str(REMOTE_RUNNER)
        )
        await environment.upload_file(
            self._benchmark_root / "elisp" / "install-deps.el", str(REMOTE_INSTALLER)
        )

        if self._elpa_bundle:
            if not self._elpa_bundle.is_dir():
                raise ValueError(f"ELPA bundle is not a directory: {self._elpa_bundle}")
            await environment.upload_dir(self._elpa_bundle, str(REMOTE_ELPA))
            await self.exec_as_agent(
                environment,
                command=(
                    f"if [ -f {REMOTE_ELPA / 'dependencies.json'} ]; then "
                    f"cp {REMOTE_ELPA / 'dependencies.json'} /logs/agent/dependencies.json; "
                    f"elif [ -f {REMOTE_ELPA / 'bundle.json'} ]; then "
                    f"cp {REMOTE_ELPA / 'bundle.json'} /logs/agent/dependencies.json; "
                    "else echo 'ELPA bundle has no dependency manifest' >&2; exit 1; fi"
                ),
            )
        else:
            await self.exec_as_agent(
                environment,
                command=f"mkdir -p {REMOTE_ELPA} && emacs -Q --batch -l {REMOTE_INSTALLER}",
                env={"MAGENT_BENCH_ELPA_DIR": str(REMOTE_ELPA)},
                timeout_sec=600,
            )

        await self.exec_as_agent(
            environment,
            command=(
                f"emacs -Q --batch -L {REMOTE_ROOT / 'lisp'} "
                f"--eval \"(let ((default-directory \\\"{REMOTE_ELPA}/\\\")) "
                "(normal-top-level-add-subdirs-to-load-path))\" "
                "--eval \"(require 'magent)\" "
                "--eval \"(princ \\\"magent-load-ok\\n\\\")\""
            ),
        )

    @override
    async def run(
        self,
        instruction: str,
        environment: BaseEnvironment,
        context: AgentContext,
    ) -> None:
        if not self.model_name:
            raise ValueError("Magent requires model_name")
        if not self._has_env(self._api_key_env):
            raise ValueError(f"Missing provider credential {self._api_key_env}")

        self.logs_dir.mkdir(parents=True, exist_ok=True)
        instruction_path = self.logs_dir / "instruction.txt"
        instruction_path.write_text(instruction, encoding="utf-8")
        await environment.upload_file(
            instruction_path, str(EnvironmentPaths.agent_dir / "instruction.txt")
        )
        pwd = await self.exec_as_agent(environment, command="pwd")
        workspace = (pwd.stdout or "").strip()
        if not workspace:
            raise RuntimeError("Could not determine task workspace")

        socket = re.sub(r"[^A-Za-z0-9_-]", "-", environment.session_id)[-60:]
        env = {
            "MAGENT_BENCH_SOURCE": str(REMOTE_ROOT),
            "MAGENT_BENCH_ELPA_DIR": str(REMOTE_ELPA),
            "MAGENT_BENCH_WORKSPACE": workspace,
            "MAGENT_BENCH_INSTRUCTION_FILE": str(
                EnvironmentPaths.agent_dir / "instruction.txt"
            ),
            "MAGENT_BENCH_LOGS_DIR": str(EnvironmentPaths.agent_dir),
            "MAGENT_BENCH_PROVIDER": self._provider,
            "MAGENT_BENCH_MODEL": self.model_name,
            "MAGENT_BENCH_BASE_URL": self._base_url,
            "MAGENT_BENCH_WIRE_API": self._wire_api,
            "MAGENT_BENCH_API_KEY_ENV": self._api_key_env,
            "MAGENT_BENCH_EFFORT": self._reasoning_effort,
        }
        await self.exec_as_agent(
            environment,
            command=(
                f"emacs -Q --daemon={socket} -l {REMOTE_RUNNER}; "
                f"cleanup() {{ emacsclient -s {socket} --eval '(kill-emacs)' "
                ">/dev/null 2>&1 || true; }; trap cleanup EXIT; "
                f"emacsclient -s {socket} --eval "
                "'(magent-benchmark-run-from-environment)' "
                "2>&1 | tee /logs/agent/emacsclient.txt"
            ),
            env=env,
        )

    @override
    def populate_context_post_run(self, context: AgentContext) -> None:
        ledger_path = self.logs_dir / "magent-ledger.json"
        result_path = self.logs_dir / "magent-result.json"
        if not ledger_path.exists() or not result_path.exists():
            self.logger.warning("Magent ledger/result missing; cannot emit ATIF")
            return
        try:
            ledger = json.loads(ledger_path.read_text(encoding="utf-8"))
            result = json.loads(result_path.read_text(encoding="utf-8"))
            trajectory = ledger_to_trajectory(
                ledger,
                result,
                model_name=self.model_name,
                version=self.version() or "unknown",
                input_price_per_million=self._input_price,
                output_price_per_million=self._output_price,
                cached_input_price_per_million=self._cached_input_price,
            )
            (self.logs_dir / "trajectory.json").write_text(
                format_trajectory_json(trajectory.to_json_dict()), encoding="utf-8"
            )
        except Exception:
            self.logger.exception("Failed to convert Magent ledger to ATIF")
            return

        metrics = trajectory.final_metrics
        if metrics:
            context.n_input_tokens = metrics.total_prompt_tokens or 0
            context.n_output_tokens = metrics.total_completion_tokens or 0
            context.n_cache_tokens = metrics.total_cached_tokens or 0
            context.cost_usd = metrics.total_cost_usd
        tool_calls = sum(len(step.tool_calls or []) for step in trajectory.steps)
        context.metadata = {
            "magent_ledger": "magent-ledger.json",
            "trajectory": "trajectory.json",
            "tool_calls": tool_calls,
            "wire_api": self._wire_api,
        }
