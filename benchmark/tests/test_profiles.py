from dataclasses import replace
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
from types import SimpleNamespace

import pytest
from harbor.models.job.config import JobConfig
import yaml

from magent_benchmark import cli
from magent_benchmark.cli import prepare_config, prepare_emacs_bundle
from magent_benchmark.job import _container_proxy_url, build_job
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


def test_job_proxy_covers_the_trial_environment_without_fingerprinting_secret(
    tmp_path: Path,
) -> None:
    config, profile, suite = _config(tmp_path)
    proxy = "http://127.0.0.1:10808"
    job, fingerprint = build_job(
        config, profile, suite, "model-x", "smoke", proxy=proxy
    )

    environment = job["environment"]
    container_proxy = "http://host.docker.internal:10808"
    assert environment["env"] == {
        "MAGENT_BENCHMARK_CONTAINER_PROXY": container_proxy,
    }
    assert environment["extra_allowed_hosts"] == ["host.docker.internal"]
    assert environment["extra_docker_compose"] == [
        str(Path(__file__).resolve().parents[1] / "docker-compose.proxy.yaml")
    ]
    assert job["agent_setup_timeout_multiplier"] == 2
    assert "AgentSetupTimeoutError" in job["retry"]["include_exceptions"]
    assert proxy not in str(fingerprint)
    assert JobConfig.model_validate(job).environment.env == {
        "MAGENT_BENCHMARK_CONTAINER_PROXY": container_proxy,
    }
    overlay = yaml.safe_load(
        Path(environment["extra_docker_compose"][0]).read_text()
    )
    assert overlay["services"]["main"]["environment"] == {
        name: "${MAGENT_BENCHMARK_CONTAINER_PROXY}"
        for name in ("HTTP_PROXY", "HTTPS_PROXY", "http_proxy", "https_proxy")
    }


def test_non_loopback_proxy_uses_the_same_compose_overlay(tmp_path: Path) -> None:
    config, profile, suite = _config(tmp_path)
    proxy = "http://proxy.example:8080"
    job, _ = build_job(config, profile, suite, "model-x", "smoke", proxy=proxy)

    assert job["environment"]["env"]["MAGENT_BENCHMARK_CONTAINER_PROXY"] == proxy
    assert job["environment"]["extra_allowed_hosts"] == ["proxy.example"]
    assert job["environment"]["extra_docker_compose"] == [
        str(Path(__file__).resolve().parents[1] / "docker-compose.proxy.yaml")
    ]
    assert job["agent_setup_timeout_multiplier"] == 2


def test_proxy_url_validation_and_loopback_rewrite() -> None:
    assert _container_proxy_url("http://localhost:10808") == (
        "http://host.docker.internal:10808"
    )
    with pytest.raises(ValueError, match=r"benchmark\.proxy.*HTTP\(S\)"):
        _container_proxy_url("socks5://127.0.0.1:10808")


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
            proxy="http://127.0.0.1:10808",
        ),
    )

    output, fingerprint = prepare_config(config, check_runtime=False)

    assert output == tmp_path / "generated" / "benchmark.yaml"
    assert fingerprint == output.with_suffix(".yaml.fingerprint.json")
    job = yaml.safe_load(output.read_text())
    assert job["jobs_dir"] == str((tmp_path / "jobs").resolve())
    assert job["agents"][0]["model_name"] == "model-x"
    assert job["agents"][0]["env"]["OPENAI_API_KEY"] == "test-key"
    assert job["environment"]["env"]["MAGENT_BENCHMARK_CONTAINER_PROXY"] == (
        "http://host.docker.internal:10808"
    )
    assert "test-key" not in fingerprint.read_text()
    assert "127.0.0.1:10808" not in fingerprint.read_text()
    assert output.stat().st_mode & 0o777 == 0o600


def test_shipped_config_and_makefile_are_the_normal_user_interface() -> None:
    path = Path(__file__).resolve().parents[1] / "config.example.toml"
    config = load_config(path)
    root = config.path.parent.parent
    makefile = (root / "Makefile").read_text()
    gitignore = (root / ".gitignore").read_text()
    assert "benchmark:" in makefile
    assert "benchmark-prepare:" in makefile
    assert "benchmark-run:" in makefile
    assert "\npurge: clean" in makefile
    assert "$(MAKE) --no-print-directory benchmark-prepare" in makefile
    assert 'magent-bench prepare --config "$(BENCHMARK_CONFIG)"' in makefile
    assert 'magent-bench bench --config "$(BENCHMARK_CONFIG)"' in makefile
    assert "docker system prune" not in makefile
    assert "docker network prune" not in makefile
    for removed_target in (
        "benchmark-check",
        "benchmark-prepare-emacs",
        "benchmark-profiles",
        "benchmark-sync",
    ):
        assert f"\n{removed_target}:" not in makefile
    assert not (config.path.parent / "Makefile").exists()
    assert not (config.path.parent / "profiles.toml").exists()
    assert "/benchmark/config.toml" in gitignore
    text = config.path.read_text()
    assert ".model_names]" not in text
    assert ".agent_versions]" not in text


def _make_fixture(tmp_path: Path) -> tuple[Path, Path, Path, Path]:
    root = Path(__file__).resolve().parents[2]
    project = tmp_path / "project with spaces"
    benchmark = project / "benchmark"
    tools = tmp_path / "tools"
    project.mkdir()
    benchmark.mkdir()
    tools.mkdir()
    shutil.copyfile(root / "Makefile", project / "Makefile")
    (project / "source-files.txt").write_text("")
    (benchmark / "config.toml").write_text("# test config\n")

    log = tmp_path / "make.log"
    fake_uv = tools / "uv"
    fake_uv.write_text(
        """#!/bin/sh
printf 'uv %s\n' "$*" >> "$MAKE_TEST_LOG"
case "$*" in
  *"magent-bench prepare --config"*)
    if [ "${MAKE_TEST_FAIL:-}" = prepare ]; then exit 9; fi
    mkdir -p benchmark/generated
    : > benchmark/generated/benchmark.yaml
    ;;
esac
"""
    )
    fake_uv.chmod(0o755)
    fake_emacs = tools / "emacs"
    fake_emacs.write_text(
        """#!/bin/sh
printf 'emacs %s\n' "$*" >> "$MAKE_TEST_LOG"
"""
    )
    fake_emacs.chmod(0o755)
    return project, log, fake_uv, fake_emacs


def _run_make_benchmark(
    project: Path,
    log: Path,
    fake_uv: Path,
    fake_emacs: Path,
    *,
    fail: str = "",
) -> subprocess.CompletedProcess[str]:
    environment = os.environ.copy()
    environment.update(
        {
            "HOME": str(project / "empty home"),
            "MAKE_TEST_FAIL": fail,
            "MAKE_TEST_LOG": str(log),
        }
    )
    return subprocess.run(
        [
            "make",
            "--no-print-directory",
            "benchmark",
            f"UV={fake_uv}",
            f"EMACS={fake_emacs}",
        ],
        cwd=project,
        env=environment,
        text=True,
        capture_output=True,
        check=False,
    )


def test_make_benchmark_prepares_when_needed_then_runs(tmp_path: Path) -> None:
    project, log, fake_uv, fake_emacs = _make_fixture(tmp_path)

    result = _run_make_benchmark(project, log, fake_uv, fake_emacs)
    assert result.returncode == 0, result.stderr
    lines = log.read_text().splitlines()
    assert lines[0].startswith("emacs ")
    assert "--extra test pytest" in lines[1]
    assert "magent-bench prepare-emacs" in lines[2]
    assert "magent-bench prepare --config" in lines[3]
    assert "magent-bench bench --config" in lines[4]

    log.write_text("")
    result = _run_make_benchmark(project, log, fake_uv, fake_emacs)
    assert result.returncode == 0, result.stderr
    lines = log.read_text().splitlines()
    assert len(lines) == 1
    assert "magent-bench bench --config" in lines[0]

    prepared = project / "benchmark" / "generated" / "benchmark.yaml"
    config = project / "benchmark" / "config.toml"
    stale_time = prepared.stat().st_mtime_ns + 2_000_000_000
    os.utime(config, ns=(stale_time, stale_time))
    log.write_text("")
    result = _run_make_benchmark(project, log, fake_uv, fake_emacs)
    assert result.returncode == 0, result.stderr
    lines = log.read_text().splitlines()
    assert any("magent-bench prepare --config" in line for line in lines)
    assert "magent-bench bench --config" in lines[-1]

    prepared.unlink()
    log.write_text("")
    result = _run_make_benchmark(
        project, log, fake_uv, fake_emacs, fail="prepare"
    )
    assert result.returncode != 0
    lines = log.read_text().splitlines()
    assert any("magent-bench prepare --config" in line for line in lines)
    assert not any("magent-bench bench --config" in line for line in lines)


def test_makefile_discovers_elpa_dependency_under_home_with_spaces(
    tmp_path: Path,
) -> None:
    root = Path(__file__).resolve().parents[2]
    project = tmp_path / "project with spaces"
    home = tmp_path / "home with spaces"
    elpa = home / ".emacs.d" / "elpa"
    project.mkdir()
    (project / "source-files.txt").write_text("")
    shutil.copyfile(root / "Makefile", project / "Makefile")
    (elpa / "gptel-20260101.1").mkdir(parents=True)
    latest = elpa / "gptel-20260202.2"
    latest.mkdir()
    with (project / "Makefile").open("a") as makefile:
        makefile.write(
            "\nprint-gptel:\n\t@printf '%s\\n' \"$(GPTEL_DIR)\"\n"
        )

    result = subprocess.run(
        ["make", "--no-print-directory", "print-gptel", f"HOME={home}"],
        cwd=project,
        text=True,
        capture_output=True,
        check=False,
    )

    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == str(latest)


def _fake_docker(path: Path) -> Path:
    docker = path / "docker"
    docker.write_text(
        """#!/bin/sh
printf 'docker %s\n' "$*" >> "$MAKE_TEST_LOG"
case "$1" in
  info)
    exit 0
    ;;
  ps)
    printf '%s\n' \
      'bench-container django__django-13033__trial__env-main-1' \
      'other-container goods-wiki-api-1'
    ;;
  rm)
    ;;
  network)
    case "$2" in
      ls)
        printf '%s\n' \
          'bench-network django__django-13033__trial__env_default' \
          'other-network goods-wiki_default'
        ;;
      rm)
        ;;
    esac
    ;;
  image)
    case "$2" in
      ls)
        printf '%s\n' \
          'env-image django__django-13033__trial__env-main' \
          'base-image swebench/sweb.eval.x86_64.django-13033' \
          'harbor-image hb__agent-cache' \
          'other-image goods-wiki-api'
        ;;
      rm)
        ;;
    esac
    ;;
esac
"""
    )
    docker.chmod(0o755)
    return docker


def _run_make_cleanup(
    project: Path,
    target: str,
    docker: Path,
    log: Path,
    home: Path,
) -> subprocess.CompletedProcess[str]:
    environment = os.environ.copy()
    environment.update({"HOME": str(home), "MAKE_TEST_LOG": str(log)})
    return subprocess.run(
        [
            "make",
            "--no-print-directory",
            target,
            f"DOCKER={docker}",
        ],
        cwd=project,
        env=environment,
        text=True,
        capture_output=True,
        check=False,
    )


def test_make_clean_and_purge_target_only_benchmark_docker_state(
    tmp_path: Path,
) -> None:
    project, log, _fake_uv, _fake_emacs = _make_fixture(tmp_path)
    docker = _fake_docker(tmp_path / "tools")
    benchmark = project / "benchmark"
    home = tmp_path / "home with spaces"
    home.mkdir()
    for path in (
        benchmark / ".venv" / "marker",
        benchmark / "generated" / "benchmark.yaml",
        benchmark / "runtime" / "marker",
        benchmark / "jobs" / "kept.json",
        benchmark / "reports" / "kept.json",
    ):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("test\n")

    result = _run_make_cleanup(project, "clean", docker, log, home)
    assert result.returncode == 0, result.stderr
    calls = log.read_text().splitlines()
    assert "docker rm -f bench-container" in calls
    assert "docker network rm bench-network" in calls
    assert not any("other-container" in call for call in calls)
    assert not any("other-network" in call for call in calls)
    assert not any(call.startswith("docker image ") for call in calls)
    assert "Removing benchmark container django__django-13033__trial__env-main-1" in result.stdout
    assert "Removing benchmark network django__django-13033__trial__env_default" in result.stdout
    assert not (benchmark / ".venv").exists()
    assert not (benchmark / "generated").exists()
    assert not (benchmark / "runtime").exists()
    assert (benchmark / "config.toml").is_file()
    assert (benchmark / "jobs" / "kept.json").is_file()
    assert (benchmark / "reports" / "kept.json").is_file()

    harbor_cache = home / ".cache" / "harbor"
    harbor_cache.mkdir(parents=True)
    (harbor_cache / "cached-task").write_text("test\n")
    log.write_text("")
    result = _run_make_cleanup(project, "purge", docker, log, home)
    assert result.returncode == 0, result.stderr
    calls = log.read_text().splitlines()
    assert "docker image rm -f env-image" in calls
    assert "docker image rm -f base-image" in calls
    assert "docker image rm -f harbor-image" in calls
    assert "docker image rm -f other-image" not in calls
    assert not harbor_cache.exists()
    assert (benchmark / "config.toml").is_file()
    assert (benchmark / "jobs" / "kept.json").is_file()
    assert (benchmark / "reports" / "kept.json").is_file()


def test_make_clean_skips_unavailable_docker_but_cleans_local_files(
    tmp_path: Path,
) -> None:
    project, log, _fake_uv, _fake_emacs = _make_fixture(tmp_path)
    marker = project / "benchmark" / "runtime" / "marker"
    marker.parent.mkdir(parents=True)
    marker.write_text("test\n")
    missing_docker = tmp_path / "tools" / "missing-docker"

    result = _run_make_cleanup(
        project,
        "clean",
        missing_docker,
        log,
        tmp_path / "home",
    )

    assert result.returncode == 0, result.stderr
    assert "Docker unavailable; skipping" in result.stdout
    assert not marker.parent.exists()


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
    monkeypatch.setenv("PYTHONWARNINGS", "default::DeprecationWarning")
    monkeypatch.delenv("TTY_INTERACTIVE", raising=False)

    def fake_run(command, *, environment, job_dir, job):
        calls["command"] = command
        calls["env"] = environment
        calls["runner_job_dir"] = job_dir
        calls["job"] = job
        return 0

    monkeypatch.setattr(cli, "_run_harbor_job", fake_run)

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
    assert calls["env"]["PYTHONWARNINGS"] == (
        "default::DeprecationWarning,"
        "ignore:Run-specific allowlist host:UserWarning"
    )
    assert "TTY_INTERACTIVE" not in calls["env"]
    assert calls["job_dir"] == tmp_path / "jobs" / (
        "magent-bench-main-model-x-suite-smoke-run"
    )
    assert calls["runner_job_dir"] == calls["job_dir"]
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
        cli,
        "_run_harbor_job",
        lambda _command, *, environment, job_dir, job: 7,
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

    monkeypatch.setattr(
        cli,
        "_run_harbor_job",
        lambda _command, *, environment, job_dir, job: 130,
    )

    import magent_benchmark.report as report

    def no_results(_job_dir: Path, _report_dir: Path):
        raise ValueError("no Harbor results")

    monkeypatch.setattr(report, "write_reports", no_results)

    assert cli.cmd_bench(SimpleNamespace(config=config.path)) == 130
    assert "Benchmark incomplete; inspect" in capsys.readouterr().err


def test_agent_progress_groups_completed_running_and_errors(tmp_path: Path) -> None:
    job = {
        "n_attempts": 3,
        "agents": [
            {"name": "opencode"},
            {"import_path": "magent_benchmark.harbor_agent:MagentHarborAgent"},
        ],
        "datasets": [{"task_names": ["one", "two"]}],
    }
    specs = cli._agent_progress_specs(job)
    assert specs == [("opencode", "OpenCode", 6), ("magent", "Magent", 6)]

    completed = tmp_path / "completed"
    completed.mkdir()
    (completed / "config.json").write_text(
        json.dumps({"agent": {"name": "opencode"}})
    )
    (completed / "result.json").write_text(
        json.dumps(
            {
                "exception_info": None,
                "agent_result": {
                    "n_input_tokens": 120,
                    "n_cache_tokens": 80,
                    "n_output_tokens": 30,
                },
            }
        )
    )

    live_opencode = tmp_path / "live-opencode"
    (live_opencode / "agent").mkdir(parents=True)
    (live_opencode / "config.json").write_text(
        json.dumps({"agent": {"name": "opencode"}})
    )
    (live_opencode / "agent" / "opencode.txt").write_text(
        json.dumps(
            {
                "type": "step_finish",
                "part": {
                    "tokens": {
                        "input": 10,
                        "output": 2,
                        "cache": {"read": 40},
                    }
                },
            }
        )
        + "\n"
    )

    failed = tmp_path / "failed"
    failed.mkdir()
    magent_agent = {
        "import_path": "magent_benchmark.harbor_agent:MagentHarborAgent"
    }
    (failed / "config.json").write_text(json.dumps({"agent": magent_agent}))
    (failed / "result.json").write_text(
        json.dumps(
            {
                "exception_info": {"exception_type": "AgentError"},
                "agent_result": {
                    "n_input_tokens": 7,
                    "n_cache_tokens": 11,
                    "n_output_tokens": 3,
                },
            }
        )
    )

    running = tmp_path / "running"
    (running / "agent").mkdir(parents=True)
    (running / "config.json").write_text(json.dumps({"agent": magent_agent}))
    (running / "agent" / "magent-progress.json").write_text(
        json.dumps(
            {
                "usage-samples": [
                    {"input": 4, "cached": 5, "output": 2},
                ]
            }
        )
    )

    snapshot = cli._agent_progress_snapshot(tmp_path, specs)
    assert snapshot["opencode"]["completed"] == 1
    assert snapshot["opencode"]["running"] == 1
    assert snapshot["opencode"]["errors"] == 0
    assert snapshot["opencode"]["input_tokens"] == 170
    assert snapshot["opencode"]["cache_tokens"] == 120
    assert snapshot["opencode"]["output_tokens"] == 32
    assert snapshot["opencode"]["last_activity"] is not None
    assert snapshot["magent"]["completed"] == 1
    assert snapshot["magent"]["running"] == 1
    assert snapshot["magent"]["errors"] == 1
    assert snapshot["magent"]["input_tokens"] == 11
    assert snapshot["magent"]["cache_tokens"] == 16
    assert snapshot["magent"]["output_tokens"] == 5
    assert snapshot["magent"]["last_activity"] is not None


def test_codex_live_usage_ignores_an_incomplete_json_line(tmp_path: Path) -> None:
    log = tmp_path / "codex.txt"
    log.write_text(
        json.dumps(
            {
                "type": "turn.completed",
                "usage": {
                    "input_tokens": 100,
                    "cached_input_tokens": 80,
                    "output_tokens": 20,
                },
            }
        )
        + "\n{incomplete"
    )

    assert cli._codex_live_usage(log) == (100, 80, 20)


def test_progress_state_shows_tokens_and_last_event() -> None:
    state = cli._empty_agent_progress()
    state.update(
        {
            "running": 2,
            "errors": 1,
            "input_tokens": 1_500,
            "cache_tokens": 1_200,
            "output_tokens": 25,
            "last_activity": 90.0,
        }
    )

    assert cli._progress_state_text(state, now=100.0) == (
        "run2 err1 · tok i1.5K/c1.2K/o25 · seen10s"
    )


def test_harbor_runner_renders_overall_and_each_agent(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    job = {
        "agents": [
            {"name": "opencode"},
            {"import_path": "magent_benchmark.harbor_agent:MagentHarborAgent"},
        ],
        "datasets": [{"task_names": ["one"]}],
    }

    assert cli._run_harbor_job(
        [sys.executable, "-c", "pass"],
        environment={},
        job_dir=tmp_path,
        job=job,
    ) == 0

    output = capsys.readouterr().out
    assert "Overall" in output
    assert "OpenCode" in output
    assert "Magent" in output
    assert "tok i0/c0/o0" in output


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
