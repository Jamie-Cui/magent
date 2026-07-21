"""Load and validate benchmark profiles without selecting a hidden default."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import os
import tomllib
from typing import Any


AGENTS = ("codex", "opencode", "magent")
STAGES = {
    "smoke": (1, 1),
    "pilot": (10, 1),
    "full": (30, 3),
}


class ConfigurationError(ValueError):
    """Raised when a run would violate benchmark comparability rules."""


@dataclass(frozen=True)
class Profile:
    name: str
    scoreboard: str
    provider: str
    wire_api: str
    base_url: str
    api_key_env: str
    agents: tuple[str, ...]
    model_names: dict[str, str]
    agent_versions: dict[str, str]
    unsupported: dict[str, str]

    def rendered_models(self, model: str) -> dict[str, str]:
        model = model.strip()
        if not model:
            raise ConfigurationError("--model must be a non-empty canonical model ID")
        return {
            agent: self.model_names[agent].format(model=model)
            for agent in self.agents
        }

    def validate(self) -> None:
        if self.scoreboard not in {"main", "compatibility"}:
            raise ConfigurationError(
                f"profile {self.name!r}: scoreboard must be main or compatibility"
            )
        if self.wire_api not in {"responses", "chat_completions"}:
            raise ConfigurationError(
                f"profile {self.name!r}: unsupported wire_api {self.wire_api!r}"
            )
        unknown = set(self.agents) - set(AGENTS)
        if unknown:
            raise ConfigurationError(
                f"profile {self.name!r}: unknown agents {sorted(unknown)}"
            )
        if set(self.model_names) != set(self.agents):
            raise ConfigurationError(
                f"profile {self.name!r}: model_names must exactly match agents"
            )
        if self.scoreboard == "main" and set(self.agents) != set(AGENTS):
            raise ConfigurationError(
                f"profile {self.name!r}: main scoreboard requires all three agents"
            )
        if (
            self.provider == "openai"
            and self.wire_api == "responses"
            and self.base_url.rstrip("/") != "https://api.openai.com/v1"
        ):
            raise ConfigurationError(
                "gptel selects the Responses API only for the official OpenAI host; "
                "use chat_completions for compatible proxies"
            )


@dataclass(frozen=True)
class Suite:
    name: str
    dataset: str
    ref: str
    manifest: Path
    expected_tasks: int


@dataclass(frozen=True)
class Config:
    path: Path
    profiles: dict[str, Profile]
    suites: dict[str, Suite]


def default_config_path() -> Path:
    return Path(__file__).resolve().parents[1] / "profiles.toml"


def load_config(path: Path | str | None = None) -> Config:
    config_path = Path(path or default_config_path()).resolve()
    with config_path.open("rb") as handle:
        data = tomllib.load(handle)

    profiles: dict[str, Profile] = {}
    for name, raw in data.get("profiles", {}).items():
        profile = Profile(
            name=name,
            scoreboard=str(raw["scoreboard"]),
            provider=str(raw["provider"]),
            wire_api=str(raw["wire_api"]),
            base_url=str(raw["base_url"]),
            api_key_env=str(raw["api_key_env"]),
            agents=tuple(raw["agents"]),
            model_names=dict(raw["model_names"]),
            agent_versions=dict(raw.get("agent_versions", {})),
            unsupported=dict(raw.get("unsupported", {})),
        )
        profile.validate()
        profiles[name] = profile

    suites: dict[str, Suite] = {}
    for name, raw in data.get("suites", {}).items():
        manifest = Path(raw["manifest"])
        if not manifest.is_absolute():
            manifest = config_path.parent / manifest
        suites[name] = Suite(
            name=name,
            dataset=str(raw["dataset"]),
            ref=str(raw["ref"]),
            manifest=manifest.resolve(),
            expected_tasks=int(raw["expected_tasks"]),
        )

    if not profiles or not suites:
        raise ConfigurationError("configuration needs at least one profile and suite")
    return Config(config_path, profiles, suites)


def require_profile(config: Config, name: str) -> Profile:
    try:
        return config.profiles[name]
    except KeyError as exc:
        choices = ", ".join(sorted(config.profiles))
        raise ConfigurationError(f"unknown profile {name!r}; choose one of: {choices}") from exc


def require_suite(config: Config, name: str) -> Suite:
    try:
        return config.suites[name]
    except KeyError as exc:
        choices = ", ".join(sorted(config.suites))
        raise ConfigurationError(f"unknown suite {name!r}; choose one of: {choices}") from exc


def read_manifest(suite: Suite, *, exact: bool = True) -> list[str]:
    if not suite.manifest.exists():
        raise ConfigurationError(f"manifest does not exist: {suite.manifest}")
    task_ids = [
        line.strip()
        for line in suite.manifest.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if len(task_ids) != len(set(task_ids)):
        raise ConfigurationError(f"manifest contains duplicate IDs: {suite.manifest}")
    if exact and len(task_ids) != suite.expected_tasks:
        raise ConfigurationError(
            f"{suite.name} manifest has {len(task_ids)} tasks; "
            f"freeze exactly {suite.expected_tasks} before running"
        )
    return task_ids


def check_api_key(profile: Profile) -> None:
    if not os.environ.get(profile.api_key_env):
        raise ConfigurationError(
            f"environment variable {profile.api_key_env} is required for profile "
            f"{profile.name!r}"
        )


def stage_shape(stage: str, approved_full: bool) -> tuple[int, int]:
    try:
        count, attempts = STAGES[stage]
    except KeyError as exc:
        raise ConfigurationError(f"unknown stage {stage!r}") from exc
    if stage == "full" and not approved_full:
        raise ConfigurationError(
            "full stage is deliberately gated; repeat with --approve-full after "
            "reviewing smoke and pilot artifacts"
        )
    return count, attempts


def public_profile(profile: Profile, model: str) -> dict[str, Any]:
    """Return the non-secret profile fingerprint recorded with each job."""
    return {
        "profile": profile.name,
        "scoreboard": profile.scoreboard,
        "canonical_model": model,
        "provider": profile.provider,
        "wire_api": profile.wire_api,
        "base_url": profile.base_url,
        "api_key_env": profile.api_key_env,
        "agents": list(profile.agents),
        "rendered_models": profile.rendered_models(model),
        "agent_versions": profile.agent_versions,
        "unsupported": profile.unsupported,
    }
