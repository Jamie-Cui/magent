"""Load and validate benchmark profiles without selecting a hidden default."""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
import tomllib
from typing import Any


AGENTS = ("codex", "opencode", "magent")
STAGES = {
    "smoke": (1, 1),
    "pilot": (10, 1),
    "full": (30, 3),
}
EFFORTS = ("auto", "minimal", "low", "medium", "high", "xhigh")


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
    api_key: str = field(repr=False)
    agents: tuple[str, ...]
    model_names: dict[str, str]
    agent_versions: dict[str, str]
    unsupported: dict[str, str]

    def rendered_models(self, model: str) -> dict[str, str]:
        model = model.strip()
        if not model:
            raise ConfigurationError("model must be a non-empty canonical model ID")
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
        if not self.agents:
            raise ConfigurationError(
                f"profile {self.name!r}: agents must contain at least one entry"
            )
        if len(self.agents) != len(set(self.agents)):
            raise ConfigurationError(
                f"profile {self.name!r}: agent names must be unique"
            )
        if set(self.model_names) != set(self.agents):
            raise ConfigurationError(
                f"profile {self.name!r}: model_names must exactly match agents"
            )
        unknown_versions = set(self.agent_versions) - set(self.agents)
        if unknown_versions:
            raise ConfigurationError(
                f"profile {self.name!r}: versions reference inactive agents "
                f"{sorted(unknown_versions)}"
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
class BenchmarkConfig:
    """One user-selected benchmark run from config.toml."""

    profile: str
    model: str
    suite: str
    stage: str = "smoke"
    effort: str = "auto"
    concurrency: int = 3
    approve_full: bool = False
    input_price_per_million: float | None = None
    output_price_per_million: float | None = None
    cached_input_price_per_million: float | None = None
    elpa_bundle: Path | None = None
    emacs_bundle: Path | None = None
    proxy: str | None = field(default=None, repr=False)

    def validate(self) -> None:
        if not self.model.strip():
            raise ConfigurationError(
                "set benchmark.model in config.toml before running make"
            )
        if self.stage not in STAGES:
            raise ConfigurationError(
                f"benchmark.stage must be one of: {', '.join(STAGES)}"
            )
        if self.effort not in EFFORTS:
            raise ConfigurationError(
                f"benchmark.effort must be one of: {', '.join(EFFORTS)}"
            )
        if self.concurrency < 1:
            raise ConfigurationError("benchmark.concurrency must be at least 1")


@dataclass(frozen=True)
class Config:
    path: Path
    profiles: dict[str, Profile]
    suites: dict[str, Suite]
    benchmark: BenchmarkConfig | None = None


def default_config_path() -> Path:
    return Path(__file__).resolve().parents[1] / "config.toml"


def _optional_float(raw: dict[str, Any], key: str) -> float | None:
    value = raw.get(key)
    return None if value is None else float(value)


def _optional_path(raw: dict[str, Any], key: str, root: Path) -> Path | None:
    value = str(raw.get(key, "")).strip()
    if not value:
        return None
    path = Path(value).expanduser()
    return (path if path.is_absolute() else root / path).resolve()


def _profile_agent_fields(
    profile_name: str, raw_agents: Any
) -> tuple[tuple[str, ...], dict[str, str], dict[str, str]]:
    """Return normalized agent names, model templates, and versions."""
    if not isinstance(raw_agents, list) or not raw_agents:
        raise ConfigurationError(
            f"profile {profile_name!r}: agents must be a non-empty array of tables"
        )

    agents: list[str] = []
    model_names: dict[str, str] = {}
    agent_versions: dict[str, str] = {}
    allowed_keys = {"name", "model", "version"}
    for index, raw_agent in enumerate(raw_agents, start=1):
        if not isinstance(raw_agent, dict):
            raise ConfigurationError(
                f"profile {profile_name!r}: agents entry {index} must be a table"
            )
        unexpected = set(raw_agent) - allowed_keys
        if unexpected:
            raise ConfigurationError(
                f"profile {profile_name!r}: agents entry {index} has unknown keys "
                f"{sorted(unexpected)}"
            )
        name = str(raw_agent.get("name", "")).strip()
        model = str(raw_agent.get("model", "")).strip()
        if not name or not model:
            raise ConfigurationError(
                f"profile {profile_name!r}: agents entry {index} requires "
                "non-empty name and model"
            )
        if name in model_names:
            raise ConfigurationError(
                f"profile {profile_name!r}: duplicate agent {name!r}"
            )
        agents.append(name)
        model_names[name] = model
        if "version" in raw_agent:
            agent_versions[name] = str(raw_agent["version"]).strip()
    return tuple(agents), model_names, agent_versions


def load_config(path: Path | str | None = None) -> Config:
    config_path = Path(path or default_config_path()).resolve()
    with config_path.open("rb") as handle:
        data = tomllib.load(handle)

    profiles: dict[str, Profile] = {}
    for name, raw in data.get("profiles", {}).items():
        agents, model_names, agent_versions = _profile_agent_fields(
            name, raw.get("agents")
        )
        profile = Profile(
            name=name,
            scoreboard=str(raw["scoreboard"]),
            provider=str(raw["provider"]),
            wire_api=str(raw["wire_api"]),
            base_url=str(raw["base_url"]),
            api_key_env=str(raw["api_key_env"]),
            api_key=str(raw.get("api_key", "")).strip(),
            agents=agents,
            model_names=model_names,
            agent_versions=agent_versions,
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

    benchmark: BenchmarkConfig | None = None
    if raw_benchmark := data.get("benchmark"):
        benchmark = BenchmarkConfig(
            profile=str(raw_benchmark.get("profile", "")),
            model=str(raw_benchmark.get("model", "")),
            suite=str(raw_benchmark.get("suite", "")),
            stage=str(raw_benchmark.get("stage", "smoke")),
            effort=str(raw_benchmark.get("effort", "auto")),
            concurrency=int(raw_benchmark.get("concurrency", 3)),
            approve_full=bool(raw_benchmark.get("approve_full", False)),
            input_price_per_million=_optional_float(
                raw_benchmark, "input_price_per_million"
            ),
            output_price_per_million=_optional_float(
                raw_benchmark, "output_price_per_million"
            ),
            cached_input_price_per_million=_optional_float(
                raw_benchmark, "cached_input_price_per_million"
            ),
            elpa_bundle=_optional_path(
                raw_benchmark, "elpa_bundle", config_path.parent
            ),
            emacs_bundle=_optional_path(
                raw_benchmark, "emacs_bundle", config_path.parent
            ),
            proxy=str(raw_benchmark.get("proxy", "")).strip() or None,
        )

    if not profiles or not suites:
        raise ConfigurationError("configuration needs at least one profile and suite")
    return Config(config_path, profiles, suites, benchmark)


def require_benchmark(config: Config) -> BenchmarkConfig:
    if config.benchmark is None:
        raise ConfigurationError("config.toml needs a [benchmark] table")
    config.benchmark.validate()
    require_profile(config, config.benchmark.profile)
    require_suite(config, config.benchmark.suite)
    return config.benchmark


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
    if not profile.api_key:
        raise ConfigurationError(
            f"set profiles.{profile.name}.api_key in the local config.toml; "
            "config.toml is gitignored"
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
