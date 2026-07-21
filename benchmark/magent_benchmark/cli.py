"""Command-line interface for generating, running, and reporting benchmarks."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import random
import re
import shutil
import subprocess
import sys
from typing import Sequence

from .job import build_job, write_job
from .profiles import (
    ConfigurationError,
    check_api_key,
    load_config,
    read_manifest,
    require_profile,
    require_suite,
)


def _common_run_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--config", type=Path, help="profiles TOML path")
    parser.add_argument("--profile", required=True, help="provider profile name")
    parser.add_argument("--model", required=True, help="canonical model ID")
    parser.add_argument(
        "--effort",
        choices=("auto", "minimal", "low", "medium", "high", "xhigh"),
        default="auto",
        help="same reasoning-effort label for agents that expose the control",
    )
    parser.add_argument("--suite", required=True, help="suite name")
    parser.add_argument(
        "--stage", choices=("smoke", "pilot", "full"), required=True
    )
    parser.add_argument(
        "--approve-full",
        action="store_true",
        help="acknowledge review of smoke and pilot before a 30x3 run",
    )
    parser.add_argument("--concurrency", type=int, default=3)
    parser.add_argument(
        "--input-price", type=float, help="USD per million non-cached input tokens"
    )
    parser.add_argument(
        "--output-price", type=float, help="USD per million output tokens"
    )
    parser.add_argument(
        "--cached-input-price", type=float, help="USD per million cached input tokens"
    )


def _resolve(args: argparse.Namespace):
    config = load_config(args.config)
    profile = require_profile(config, args.profile)
    suite = require_suite(config, args.suite)
    return config, profile, suite


def cmd_profiles(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    payload = {
        name: {
            "scoreboard": profile.scoreboard,
            "provider": profile.provider,
            "wire_api": profile.wire_api,
            "agents": list(profile.agents),
            "unsupported": profile.unsupported,
        }
        for name, profile in sorted(config.profiles.items())
    }
    print(json.dumps(payload, indent=2, ensure_ascii=False))
    return 0


def cmd_preflight(args: argparse.Namespace) -> int:
    config, profile, suite = _resolve(args)
    profile.rendered_models(args.model)
    tasks = read_manifest(suite)
    if not args.skip_key_check:
        check_api_key(profile)
    print(
        json.dumps(
            {
                "ok": True,
                "profile": profile.name,
                "scoreboard": profile.scoreboard,
                "model": args.model,
                "suite": suite.name,
                "tasks": len(tasks),
            },
            indent=2,
            ensure_ascii=False,
        )
    )
    return 0


def _generate(args: argparse.Namespace) -> tuple[Path, Path]:
    config, profile, suite = _resolve(args)
    if args.concurrency < 1:
        raise ConfigurationError("--concurrency must be at least 1")
    check_api_key(profile)
    job, fingerprint = build_job(
        config,
        profile,
        suite,
        args.model,
        args.stage,
        effort=args.effort,
        input_price_per_million=args.input_price,
        output_price_per_million=args.output_price,
        cached_input_price_per_million=args.cached_input_price,
        approved_full=args.approve_full,
        concurrency=args.concurrency,
    )
    output = args.output or (
        config.path.parent
        / "generated"
        / f"{profile.name}-{args.model.replace('/', '-')}-{suite.name}-{args.stage}.yaml"
    )
    return write_job(output, job, fingerprint)


def cmd_generate(args: argparse.Namespace) -> int:
    output, fingerprint = _generate(args)
    print(output)
    print(fingerprint)
    return 0


def cmd_run(args: argparse.Namespace) -> int:
    output, _ = _generate(args)
    command = ["harbor", "jobs", "start", "--config", str(output), "--yes"]
    return subprocess.run(command, check=False).returncode


def _read_candidate_ids(path: Path) -> list[str]:
    ids = [
        line.strip()
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if len(ids) != len(set(ids)):
        raise ConfigurationError(f"candidate list has duplicate IDs: {path}")
    return ids


def cmd_freeze_manifest(args: argparse.Namespace) -> int:
    config = load_config(args.config)
    suite = require_suite(config, args.suite)
    candidates = _read_candidate_ids(args.from_file)
    if len(candidates) < suite.expected_tasks:
        raise ConfigurationError(
            f"need at least {suite.expected_tasks} eligible candidates, got {len(candidates)}"
        )
    selected = sorted(candidates)
    random.Random(args.seed).shuffle(selected)
    selected = selected[: suite.expected_tasks]
    rendered = (
        f"# Frozen by magent-bench; seed={args.seed}; source={args.from_file}\n"
        + "\n".join(selected)
        + "\n"
    )
    if args.approve_write:
        suite.manifest.write_text(rendered, encoding="utf-8")
        print(suite.manifest)
    else:
        print(rendered, end="")
        print(
            "dry run: pass --approve-write to replace the suite manifest",
            file=sys.stderr,
        )
    return 0


def cmd_report(args: argparse.Namespace) -> int:
    from .report import write_reports

    paths = write_reports(args.jobs_dir, args.output_dir, bootstrap=args.bootstrap)
    for path in paths:
        print(path)
    return 0


def cmd_prepare_elpa(args: argparse.Namespace) -> int:
    source = args.from_dir.expanduser().resolve()
    output = args.output.expanduser().resolve()
    if output.exists():
        raise ConfigurationError(f"output already exists; refusing to overwrite: {output}")
    if not source.is_dir():
        raise ConfigurationError(f"ELPA source directory does not exist: {source}")
    packages = (
        "gptel",
        "transient",
        "acp",
        "shell-maker",
        "agent-shell",
        "cond-let",
        "compat",
        "yaml",
        "llama",
        "with-editor",
    )
    selected: dict[str, Path] = {}
    missing: list[str] = []
    for package in packages:
        pattern = re.compile(rf"^{re.escape(package)}-[0-9].*$")
        candidates = sorted(
            (path for path in source.iterdir() if path.is_dir() and pattern.match(path.name)),
            key=lambda path: path.name,
        )
        if candidates:
            selected[package] = candidates[-1]
        else:
            missing.append(package)
    if any(package in missing for package in ("gptel", "acp", "agent-shell", "yaml")):
        raise ConfigurationError(
            "required packages missing from ELPA source: " + ", ".join(missing)
        )
    output.mkdir(parents=True)
    for path in selected.values():
        shutil.copytree(path, output / path.name, symlinks=True)
    (output / "bundle.json").write_text(
        json.dumps(
            {
                "source": str(source),
                "packages": {name: path.name for name, path in selected.items()},
                "not_present": missing,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    print(output)
    if missing:
        print(
            "not copied (may be built in or still need installation): " + ", ".join(missing),
            file=sys.stderr,
        )
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="magent-bench")
    sub = parser.add_subparsers(dest="command", required=True)

    profiles = sub.add_parser("profiles", help="list provider profiles")
    profiles.add_argument("--config", type=Path)
    profiles.set_defaults(func=cmd_profiles)

    preflight = sub.add_parser("preflight", help="validate a scored-run selection")
    preflight.add_argument("--config", type=Path)
    preflight.add_argument("--profile", required=True)
    preflight.add_argument("--model", required=True)
    preflight.add_argument("--suite", required=True)
    preflight.add_argument("--skip-key-check", action="store_true")
    preflight.set_defaults(func=cmd_preflight)

    generate = sub.add_parser("generate", help="write a gated Harbor job YAML")
    _common_run_arguments(generate)
    generate.add_argument("--output", type=Path)
    generate.set_defaults(func=cmd_generate)

    run = sub.add_parser("run", help="generate and start a Harbor job")
    _common_run_arguments(run)
    run.add_argument("--output", type=Path)
    run.set_defaults(func=cmd_run)

    freeze = sub.add_parser(
        "freeze-manifest", help="deterministically select from eligible task IDs"
    )
    freeze.add_argument("--config", type=Path)
    freeze.add_argument("--suite", required=True)
    freeze.add_argument("--from-file", type=Path, required=True)
    freeze.add_argument("--seed", type=int, default=20260720)
    freeze.add_argument("--approve-write", action="store_true")
    freeze.set_defaults(func=cmd_freeze_manifest)

    report = sub.add_parser("report", help="summarize Harbor results without a composite")
    report.add_argument("jobs_dir", type=Path)
    report.add_argument("--output-dir", type=Path, default=Path("reports"))
    report.add_argument("--bootstrap", type=int, default=10_000)
    report.set_defaults(func=cmd_report)

    prepare = sub.add_parser(
        "prepare-elpa", help="copy exact local Magent dependencies into a run bundle"
    )
    prepare.add_argument("--from-dir", type=Path, default=Path("~/.emacs.d/elpa"))
    prepare.add_argument("--output", type=Path, required=True)
    prepare.set_defaults(func=cmd_prepare_elpa)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return int(args.func(args))
    except (ConfigurationError, OSError, ValueError) as exc:
        parser.error(str(exc))
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
