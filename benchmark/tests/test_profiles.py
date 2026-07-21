from dataclasses import replace
from pathlib import Path

import pytest
from harbor.models.job.config import JobConfig

from magent_benchmark.job import build_job
from magent_benchmark.profiles import (
    ConfigurationError,
    Config,
    Profile,
    Suite,
    load_config,
    stage_shape,
)


def _config(tmp_path: Path) -> tuple[Config, Profile, Suite]:
    manifest = tmp_path / "tasks.txt"
    manifest.write_text("".join(f"task-{index}\n" for index in range(30)))
    profile = Profile(
        name="main",
        scoreboard="main",
        provider="openai",
        wire_api="responses",
        base_url="https://api.openai.com/v1",
        api_key_env="OPENAI_API_KEY",
        agents=("codex", "opencode", "magent"),
        model_names={
            "codex": "{model}",
            "opencode": "openai/{model}",
            "magent": "{model}",
        },
        agent_versions={},
        unsupported={},
    )
    suite = Suite("suite", "org/dataset", "7", manifest, 30)
    config = Config(tmp_path / "profiles.toml", {"main": profile}, {"suite": suite})
    return config, profile, suite


def test_shipped_profiles_separate_main_and_compatibility() -> None:
    config = load_config()
    assert config.profiles["openai-main"].agents == ("codex", "opencode", "magent")
    deepseek = config.profiles["deepseek-compatibility"]
    assert deepseek.scoreboard == "compatibility"
    assert deepseek.agents == ("opencode", "magent")
    assert "codex" in deepseek.unsupported


def test_full_stage_requires_manual_acknowledgement() -> None:
    with pytest.raises(ConfigurationError, match="--approve-full"):
        stage_shape("full", False)
    assert stage_shape("full", True) == (30, 3)


def test_job_uses_same_canonical_model_and_disables_web(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    job, fingerprint = build_job(
        config, profile, suite, "model-x", "pilot", effort="medium"
    )

    assert job["n_attempts"] == 1
    assert job["datasets"][0]["task_names"] == [f"task-{i}" for i in range(10)]
    codex, opencode, magent = job["agents"]
    assert codex["model_name"] == "model-x"
    assert codex["kwargs"]["web_search"] == "disabled"
    assert codex["kwargs"]["reasoning_effort"] == "medium"
    assert opencode["model_name"] == "openai/model-x"
    assert opencode["kwargs"]["opencode_config"]["permission"]["websearch"] == "deny"
    assert opencode["kwargs"]["variant"] == "medium"
    assert magent["model_name"] == "model-x"
    assert magent["kwargs"]["reasoning_effort"] == "medium"
    assert fingerprint["canonical_model"] == "model-x"
    assert fingerprint["reasoning_effort"] == "medium"
    assert JobConfig.model_validate(job).n_concurrent_trials == 3


def test_auto_effort_does_not_inherit_harbor_codex_high_default(
    tmp_path: Path,
) -> None:
    config, profile, suite = _config(tmp_path)
    job, _ = build_job(config, profile, suite, "model-x", "smoke")
    assert job["agents"][0]["kwargs"]["reasoning_effort"] is None


def test_full_stage_requires_explicit_prices(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    profile = replace(
        profile,
        agent_versions={"codex": "1.0.0", "opencode": "1.0.0"},
    )
    with pytest.raises(ValueError, match="--input-price and --output-price"):
        build_job(
            config,
            profile,
            suite,
            "model-x",
            "full",
            approved_full=True,
        )


def test_negative_token_price_is_rejected(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    with pytest.raises(ValueError, match="non-negative"):
        build_job(
            config,
            profile,
            suite,
            "model-x",
            "smoke",
            input_price_per_million=-1,
        )


def test_full_stage_records_frozen_runtime_and_pricing(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    config, profile, suite = _config(tmp_path)
    profile = replace(
        profile,
        agent_versions={"codex": "1.0.0", "opencode": "1.0.0"},
    )
    elpa = tmp_path / "elpa"
    elpa.mkdir()
    (elpa / "bundle.json").write_text("{}\n")
    emacs = tmp_path / "emacs"
    (emacs / "bin").mkdir(parents=True)
    for binary in ("emacs", "emacsclient"):
        (emacs / "bin" / binary).write_text("placeholder")
    monkeypatch.setenv("MAGENT_BENCH_ELPA_BUNDLE", str(elpa))
    monkeypatch.setenv("MAGENT_BENCH_EMACS_BUNDLE", str(emacs))

    job, fingerprint = build_job(
        config,
        profile,
        suite,
        "model-x",
        "full",
        input_price_per_million=2,
        output_price_per_million=10,
        cached_input_price_per_million=0.5,
        approved_full=True,
    )

    magent = job["agents"][2]
    assert job["n_attempts"] == 3
    assert magent["kwargs"]["elpa_bundle"] == str(elpa)
    assert magent["kwargs"]["emacs_bundle"] == str(emacs)
    assert magent["kwargs"]["input_price_per_million"] == 2
    assert fingerprint["pricing_usd_per_million_tokens"] == {
        "input_price_per_million": 2,
        "output_price_per_million": 10,
        "cached_input_price_per_million": 0.5,
    }
    assert JobConfig.model_validate(job).n_attempts == 3


def test_custom_responses_proxy_fails_closed() -> None:
    profile = Profile(
        name="bad",
        scoreboard="compatibility",
        provider="openai",
        wire_api="responses",
        base_url="https://proxy.example/v1",
        api_key_env="KEY",
        agents=("magent",),
        model_names={"magent": "{model}"},
        agent_versions={},
        unsupported={},
    )
    with pytest.raises(ConfigurationError, match="official OpenAI host"):
        profile.validate()
