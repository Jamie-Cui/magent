"""Generate a Harbor 0.20 job from a frozen benchmark profile."""

from __future__ import annotations

from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
from urllib.parse import urlparse
from typing import Any

import yaml

from .profiles import (
    Config,
    Profile,
    Suite,
    public_profile,
    read_manifest,
    stage_shape,
)


def _agent_config(
    profile: Profile, model: str, effort: str, agent: str
) -> dict[str, Any]:
    rendered = profile.rendered_models(model)
    host = urlparse(profile.base_url).hostname
    result: dict[str, Any] = {
        "model_name": rendered[agent],
        "env": {profile.api_key_env: profile.api_key},
    }
    if host:
        result["extra_allowed_hosts"] = [host]

    version = profile.agent_versions.get(agent, "").strip()
    if version:
        result["kwargs"] = {"version": version}

    if agent == "codex":
        result["name"] = "codex"
        result.setdefault("kwargs", {}).update(
            {
                "web_search": "disabled",
                "reasoning_summary": "auto",
                # Harbor's Codex adapter otherwise injects its own "high"
                # default. An explicit null preserves provider/model default
                # semantics when the benchmark effort is "auto".
                "reasoning_effort": None if effort == "auto" else effort,
            }
        )
    elif agent == "opencode":
        result["name"] = "opencode"
        result.setdefault("kwargs", {})["opencode_config"] = {
            "permission": {
                "*": "allow",
                "webfetch": "deny",
                "websearch": "deny",
                "question": "deny",
            }
        }
        if effort != "auto":
            result["kwargs"]["variant"] = effort
    elif agent == "magent":
        result["import_path"] = (
            "magent_benchmark.harbor_agent:MagentHarborAgent"
        )
        result.setdefault("kwargs", {}).update(
            {
                "provider": profile.provider,
                "base_url": profile.base_url,
                "wire_api": profile.wire_api,
                "api_key_env": profile.api_key_env,
                "reasoning_effort": effort,
            }
        )
    else:  # guarded by Profile.validate, kept fail-closed here too
        raise ValueError(f"unsupported agent {agent!r}")
    return result


def build_job(
    config: Config,
    profile: Profile,
    suite: Suite,
    model: str,
    stage: str,
    *,
    effort: str = "auto",
    input_price_per_million: float | None = None,
    output_price_per_million: float | None = None,
    cached_input_price_per_million: float | None = None,
    approved_full: bool = False,
    concurrency: int = 3,
    elpa_bundle: str | Path | None = None,
    emacs_bundle: str | Path | None = None,
) -> tuple[dict[str, Any], dict[str, Any]]:
    task_count, attempts = stage_shape(stage, approved_full)
    prices = {
        "input_price_per_million": input_price_per_million,
        "output_price_per_million": output_price_per_million,
        "cached_input_price_per_million": cached_input_price_per_million,
    }
    if any(value is not None and value < 0 for value in prices.values()):
        raise ValueError("token prices must be non-negative")
    if stage == "full":
        unpinned = [
            agent
            for agent in profile.agents
            if agent != "magent" and not profile.agent_versions.get(agent, "").strip()
        ]
        if unpinned:
            raise ValueError(
                "full stage requires pinned agent_versions for: " + ", ".join(unpinned)
            )
        if suite.ref in {"", "latest", "head"}:
            raise ValueError(
                f"full stage requires an immutable dataset ref, got {suite.ref!r}"
            )
        if input_price_per_million is None or output_price_per_million is None:
            raise ValueError(
                "full stage requires --input-price and --output-price for cost reporting"
            )
    manifest = read_manifest(suite)
    selected = manifest[:task_count]
    generated_at = datetime.now(timezone.utc).isoformat()
    manifest_sha = hashlib.sha256(
        ("\n".join(manifest) + "\n").encode("utf-8")
    ).hexdigest()
    slug = model.replace("/", "-").replace(" ", "-")

    job = {
        "job_name": f"magent-bench-{profile.name}-{slug}-{suite.name}-{stage}",
        "n_attempts": attempts,
        "n_concurrent_trials": concurrency,
        "retry": {
            "max_retries": 1,
            "include_exceptions": [
                "ApiRateLimitError",
                "ApiInternalServerError",
                "ApiOverloadedError",
                "ApiConnectionClosedError",
                "ApiResponseStalledError",
                "NetworkConnectionError",
            ],
        },
        "agents": [
            _agent_config(profile, model, effort, agent) for agent in profile.agents
        ],
        "datasets": [
            {
                "name": suite.dataset,
                "ref": suite.ref,
                "task_names": selected,
            }
        ],
    }
    magent = next(agent for agent in job["agents"] if agent.get("import_path"))
    magent.setdefault("kwargs", {}).update(
        {key: value for key, value in prices.items() if value is not None}
    )
    if elpa_bundle:
        bundle_path = Path(elpa_bundle).expanduser().resolve()
        if not bundle_path.is_dir():
            raise ValueError(f"ELPA bundle is not a directory: {bundle_path}")
        if stage == "full" and not (bundle_path / "bundle.json").is_file():
            raise ValueError(
                "full stage requires bundle.json in benchmark.elpa_bundle; "
                "create it with magent-bench prepare-elpa"
            )
        elpa_bundle = str(bundle_path)
        magent.setdefault("kwargs", {})["elpa_bundle"] = str(bundle_path)
    elif stage == "full":
        raise ValueError(
            "full stage requires benchmark.elpa_bundle so Elisp dependencies are frozen"
        )
    if emacs_bundle:
        emacs_path = Path(emacs_bundle).expanduser().resolve()
        missing_bins = [
            name
            for name in ("emacs", "emacsclient")
            if not (emacs_path / "bin" / name).is_file()
        ]
        if missing_bins:
            raise ValueError(
                "Emacs bundle is missing: "
                + ", ".join(f"bin/{name}" for name in missing_bins)
            )
        emacs_bundle = str(emacs_path)
        magent.setdefault("kwargs", {})["emacs_bundle"] = str(emacs_path)
    elif stage == "full":
        raise ValueError(
            "full stage requires benchmark.emacs_bundle so Emacs is frozen "
            "and does not depend on each task image's package repository"
        )
    fingerprint = {
        "schema_version": 1,
        "generated_at": generated_at,
        "stage": stage,
        "suite": suite.name,
        "dataset": suite.dataset,
        "dataset_ref": suite.ref,
        "task_count": task_count,
        "attempts": attempts,
        "reasoning_effort": effort,
        "pricing_usd_per_million_tokens": prices,
        "manifest": str(suite.manifest.relative_to(config.path.parent)),
        "manifest_sha256": manifest_sha,
        "tasks": selected,
        "elpa_bundle": elpa_bundle,
        "emacs_bundle": emacs_bundle,
        **public_profile(profile, model),
    }
    return job, fingerprint


def write_job(
    output: Path,
    job: dict[str, Any],
    fingerprint: dict[str, Any],
) -> tuple[Path, Path]:
    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        yaml.safe_dump(job, sort_keys=False, allow_unicode=True), encoding="utf-8"
    )
    output.chmod(0o600)
    fingerprint_path = output.with_suffix(output.suffix + ".fingerprint.json")
    fingerprint_path.write_text(
        json.dumps(fingerprint, indent=2, ensure_ascii=False, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return output, fingerprint_path
