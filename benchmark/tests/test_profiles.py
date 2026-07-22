from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace

import pytest
from harbor.models.job.config import JobConfig
import yaml

from magent_benchmark import cli
from magent_benchmark.cli import prepare_config, prepare_emacs_bundle
from magent_benchmark.job import build_job
from magent_benchmark.profiles import (
    BenchmarkConfig,
    ConfigurationError,
    Config,
    Profile,
    Suite,
    check_api_key,
    load_config,
    require_benchmark,
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
        api_key="test-key",
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
    config = Config(tmp_path / "config.toml", {"main": profile}, {"suite": suite})
    return config, profile, suite


def test_shipped_profiles_separate_main_and_compatibility() -> None:
    path = Path(__file__).resolve().parents[1] / "config.example.toml"
    config = load_config(path)
    assert config.path.name == "config.example.toml"
    assert config.benchmark is not None
    assert config.profiles["openai-main"].agents == ("codex", "opencode", "magent")
    deepseek = config.profiles["deepseek-compatibility"]
    assert deepseek.scoreboard == "compatibility"
    assert deepseek.agents == ("opencode", "magent")
    assert "codex" in deepseek.unsupported
    assert not deepseek.api_key


def test_profile_agent_can_be_disabled_by_commenting_one_line(
    tmp_path: Path,
) -> None:
    source = Path(__file__).resolve().parents[1] / "config.example.toml"
    text = source.read_text()
    agent_line = (
        '  { name = "opencode", model = "deepseek/{model}", version = "" },'
    )
    assert text.count(agent_line) == 1
    path = tmp_path / "config.toml"
    path.write_text(text.replace(agent_line, f"  # {agent_line.lstrip()}"))

    profile = load_config(path).profiles["deepseek-compatibility"]

    assert profile.agents == ("magent",)
    assert profile.model_names == {"magent": "{model}"}
    assert profile.agent_versions == {}


def test_profile_rejects_duplicate_inline_agent_entries(tmp_path: Path) -> None:
    source = Path(__file__).resolve().parents[1] / "config.example.toml"
    text = source.read_text()
    original = (
        '  { name = "opencode", model = "deepseek/{model}", version = "" },\n'
        '  { name = "magent", model = "{model}" },'
    )
    duplicate = (
        '  { name = "magent", model = "deepseek/{model}", version = "" },\n'
        '  { name = "magent", model = "{model}" },'
    )
    assert original in text
    path = tmp_path / "config.toml"
    path.write_text(text.replace(original, duplicate))

    with pytest.raises(ConfigurationError, match="duplicate agent 'magent'"):
        load_config(path)


def test_benchmark_selection_requires_model(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    config = Config(
        config.path,
        config.profiles,
        config.suites,
        BenchmarkConfig(profile=profile.name, model="", suite=suite.name),
    )

    with pytest.raises(ConfigurationError, match="benchmark.model"):
        require_benchmark(config)


def test_full_stage_requires_manual_acknowledgement() -> None:
    with pytest.raises(ConfigurationError, match="--approve-full"):
        stage_shape("full", False)
    assert stage_shape("full", True) == (30, 3)


def test_job_uses_same_canonical_model_and_disables_web(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    job, fingerprint = build_job(
        config, profile, suite, "model-x", "pilot", effort="medium"
    )

    assert all(
        agent["env"]["OPENAI_API_KEY"] == "test-key" for agent in job["agents"]
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
    assert "test-key" not in repr(profile)
    assert "test-key" not in str(fingerprint)
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


def test_full_stage_records_frozen_runtime_and_pricing(tmp_path: Path) -> None:
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
        elpa_bundle=elpa,
        emacs_bundle=emacs,
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


def test_api_key_must_be_set_in_local_config(tmp_path: Path) -> None:
    _, profile, _ = _config(tmp_path)

    with pytest.raises(
        ConfigurationError, match=r"profiles\.main\.api_key.*config\.toml"
    ):
        check_api_key(replace(profile, api_key=""))


def test_config_driven_prepare_writes_single_harbor_job(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    config = Config(
        config.path,
        config.profiles,
        config.suites,
        BenchmarkConfig(
            profile=profile.name,
            model="model-x",
            suite=suite.name,
            stage="smoke",
            effort="medium",
        ),
    )

    output, fingerprint = prepare_config(config, check_runtime=False)

    assert output == tmp_path / "generated" / "benchmark.yaml"
    assert fingerprint == output.with_suffix(".yaml.fingerprint.json")
    job = yaml.safe_load(output.read_text())
    assert job["jobs_dir"] == str((tmp_path / "jobs").resolve())
    assert job["agents"][0]["model_name"] == "model-x"
    assert job["agents"][0]["env"]["OPENAI_API_KEY"] == "test-key"
    assert "test-key" not in fingerprint.read_text()
    assert output.stat().st_mode & 0o777 == 0o600


def test_shipped_config_and_makefile_are_the_normal_user_interface() -> None:
    path = Path(__file__).resolve().parents[1] / "config.example.toml"
    config = load_config(path)
    makefile = (config.path.parent / "Makefile").read_text()
    gitignore = (config.path.parent.parent / ".gitignore").read_text()
    assert ".DEFAULT_GOAL := prepare" in makefile
    assert "magent-bench prepare --config $(CONFIG)" in makefile
    assert "magent-bench bench --config $(CONFIG)" in makefile
    assert not (config.path.parent / "profiles.toml").exists()
    assert "/benchmark/config.toml" in gitignore
    text = config.path.read_text()
    assert ".model_names]" not in text
    assert ".agent_versions]" not in text


def test_emacs_bundle_build_uses_config_proxy_without_leaking_it(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    dockerfile = tmp_path / "emacs-bundle.Dockerfile"
    dockerfile.write_text("FROM scratch\n")
    output = tmp_path / "runtime" / "emacs"
    calls: dict[str, object] = {}

    def fake_run(command, *, env, check):
        calls["command"] = command
        calls["env"] = env
        for name in ("emacs", "emacsclient"):
            binary = output / "bin" / name
            binary.parent.mkdir(parents=True, exist_ok=True)
            binary.write_text("placeholder")
        return SimpleNamespace(returncode=0)

    monkeypatch.setattr(cli.subprocess, "run", fake_run)

    proxy = "http://127.0.0.1:10808"
    assert prepare_emacs_bundle(dockerfile, output, proxy=proxy) == output

    command = calls["command"]
    environment = calls["env"]
    assert isinstance(command, list)
    assert isinstance(environment, dict)
    assert proxy not in command
    assert environment["HTTPS_PROXY"] == proxy
    assert command[command.index("--network") + 1] == "host"


def test_config_driven_bench_runs_prepared_job_and_writes_report(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    config, profile, suite = _config(tmp_path)
    config.path.write_text("# test config\n")
    config = Config(
        config.path,
        config.profiles,
        config.suites,
        BenchmarkConfig(profile.name, "model-x", suite.name),
    )
    output, _ = prepare_config(config, check_runtime=False)
    calls: dict[str, object] = {}

    monkeypatch.setattr(cli, "load_config", lambda _path: config)
    monkeypatch.setattr(cli, "_check_local_runtime", lambda: None)
    monkeypatch.setattr(
        cli, "_fresh_job_name", lambda _name: "magent-bench-main-model-x-suite-smoke-run"
    )

    def fake_run(command, check=False):
        calls["command"] = command
        return SimpleNamespace(returncode=0)

    monkeypatch.setattr(cli.subprocess, "run", fake_run)

    import magent_benchmark.report as report

    def fake_reports(job_dir: Path, report_dir: Path):
        calls["job_dir"] = job_dir
        calls["report_dir"] = report_dir
        return [report_dir / "report.md"]

    monkeypatch.setattr(report, "write_reports", fake_reports)

    assert cli.cmd_bench(SimpleNamespace(config=config.path)) == 0
    assert calls["command"] == [
        "harbor",
        "jobs",
        "start",
        "--config",
        str(output),
        "--job-name",
        "magent-bench-main-model-x-suite-smoke-run",
        "--quiet",
        "--yes",
    ]
    assert calls["job_dir"] == tmp_path / "jobs" / (
        "magent-bench-main-model-x-suite-smoke-run"
    )
    assert calls["report_dir"] == tmp_path / "reports" / (
        "magent-bench-main-model-x-suite-smoke-run"
    )


def test_bench_writes_partial_report_after_harbor_failure(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    config, profile, suite = _config(tmp_path)
    config.path.write_text("# test config\n")
    config = Config(
        config.path,
        config.profiles,
        config.suites,
        BenchmarkConfig(profile.name, "model-x", suite.name),
    )
    prepare_config(config, check_runtime=False)
    monkeypatch.setattr(cli, "load_config", lambda _path: config)
    monkeypatch.setattr(cli, "_check_local_runtime", lambda: None)
    monkeypatch.setattr(cli, "_fresh_job_name", lambda _name: "partial-run")
    monkeypatch.setattr(
        cli.subprocess, "run", lambda _command, check=False: SimpleNamespace(returncode=7)
    )

    import magent_benchmark.report as report

    monkeypatch.setattr(
        report,
        "write_reports",
        lambda _job_dir, report_dir: [report_dir / "report.md"],
    )

    assert cli.cmd_bench(SimpleNamespace(config=config.path)) == 7
    assert "Benchmark partial report:" in capsys.readouterr().out


def test_bench_interrupt_without_results_is_incomplete(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    config, profile, suite = _config(tmp_path)
    config.path.write_text("# test config\n")
    config = Config(
        config.path,
        config.profiles,
        config.suites,
        BenchmarkConfig(profile.name, "model-x", suite.name),
    )
    prepare_config(config, check_runtime=False)
    monkeypatch.setattr(cli, "load_config", lambda _path: config)
    monkeypatch.setattr(cli, "_check_local_runtime", lambda: None)
    monkeypatch.setattr(cli, "_fresh_job_name", lambda _name: "interrupted-run")

    def interrupt(_command, check=False):
        raise KeyboardInterrupt

    monkeypatch.setattr(cli.subprocess, "run", interrupt)

    import magent_benchmark.report as report

    def no_results(_job_dir: Path, _report_dir: Path):
        raise ValueError("no Harbor results")

    monkeypatch.setattr(report, "write_reports", no_results)

    assert cli.cmd_bench(SimpleNamespace(config=config.path)) == 130
    assert "Benchmark incomplete; inspect" in capsys.readouterr().err


def test_custom_responses_proxy_fails_closed() -> None:
    profile = Profile(
        name="bad",
        scoreboard="compatibility",
        provider="openai",
        wire_api="responses",
        base_url="https://proxy.example/v1",
        api_key_env="KEY",
        api_key="test-key",
        agents=("magent",),
        model_names={"magent": "{model}"},
        agent_versions={},
        unsupported={},
    )
    with pytest.raises(ConfigurationError, match="official OpenAI host"):
        profile.validate()
