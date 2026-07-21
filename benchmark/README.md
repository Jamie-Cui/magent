# Magent coding-agent benchmark

这个目录用 [Harbor](https://github.com/laude-institute/harbor) 0.20.0 对比完整的
Codex、OpenCode 和 Magent 产品能力。它不会把三个代理压成同一套工具或系统提示；唯一完全相同的输入是 benchmark 的用户 instruction。

## 固定的评测口径

- 两个 suite 分开报告：SWE-bench Verified 30 题和 Terminal-Bench 2.1 30 个 CPU-compatible 任务，不计算综合分。
- `smoke = 1×1`，`pilot = 10×1`，`full = 30×3`。`full` 必须显式传 `--approve-full`，并要求固定 dataset ref、Codex/OpenCode 版本、Emacs/ELPA bundle 和模型价格。
- `openai-main` 主榜必须同时包含 Codex、OpenCode、Magent，并把一个显式提供的 canonical model 映射给三者。
- `deepseek-compatibility` 是兼容榜，只跑 OpenCode 和 Magent。Codex 不支持的 provider 不做静默 fallback。
- 代理自身的 web search/web fetch 被关闭；Harbor task 定义自身的网络策略保持不变。代理执行阶段只额外放行 profile 的模型 API host。
- 三者在 task container 内使用自动权限。Magent 保留原生 gptel transport、skills、capabilities 和 child-agent tools。
- Harbor 的 verifier 是唯一 correctness oracle。只有 oracle 本身失败，或三个代理在同一题发生相同基础设施错误时，才允许从已冻结 manifest 中删题；不能补题。

## 安装

需要 Python 3.12+、`uv`、Docker，以及能安装 Emacs 29.1+ 的 Linux task image。

```bash
cd benchmark
uv sync --extra test
uv run magent-bench profiles
```

`profiles.toml` 没有默认模型。每次生成都必须选择 profile 和 model，例如：

```bash
export OPENAI_API_KEY=...
uv run magent-bench preflight \
  --profile openai-main \
  --model YOUR_OPENAI_MODEL \
  --suite swe-bench-verified

export DEEPSEEK_API_KEY=...
uv run magent-bench preflight \
  --profile deepseek-compatibility \
  --model deepseek-v4-pro \
  --suite terminal-bench-2.1
```

如果 endpoint 是 OpenAI-compatible Chat Completions proxy，请另建 `wire_api = "chat_completions"` profile。gptel 当前只能在官方 `api.openai.com` host 上由 `gptel-make-openai` 选择 Responses API；preflight 对错误组合会直接失败。

## 冻结任务

仓库不会假装随机 30 题已经审过。先从 Harbor 对应 immutable dataset ref 导出全部 task ID；Terminal-Bench 候选列表还应先排除要求 GPU 或非 CPU-compatible 的任务。然后生成确定性候选 manifest：

```bash
uv run magent-bench freeze-manifest \
  --suite swe-bench-verified \
  --from-file /path/to/eligible-swe-task-ids.txt \
  --seed 20260720

# 审阅 stdout 后才写入
uv run magent-bench freeze-manifest \
  --suite swe-bench-verified \
  --from-file /path/to/eligible-swe-task-ids.txt \
  --seed 20260720 \
  --approve-write
```

manifest 必须恰好包含 30 个唯一 ID，否则任何 run config 都不会生成。开始正式跑之前，把 `profiles.toml` 中 suite 的 `ref` 改成 Harbor 解析出的 immutable ref。

## 固定 Magent 的 Emacs/Elisp 运行时

Smoke/pilot 在没有 bundle 时可以在 container setup 阶段从 package archives 安装依赖。正式跑必须先冻结本机已安装版本：

```bash
uv run magent-bench prepare-elpa --output runtime/elpa

# 补齐本机没有、但目标 Emacs 需要的依赖，并写版本清单
MAGENT_BENCH_ELPA_DIR="$PWD/runtime/elpa" \
MAGENT_BENCH_DEPENDENCIES_FILE="$PWD/runtime/elpa/dependencies.json" \
emacs -Q --batch -l elisp/install-deps.el

export MAGENT_BENCH_ELPA_BUNDLE="$PWD/runtime/elpa"
```

不同任务镜像的软件仓库可能只提供 Emacs 27/28。Smoke/pilot 会先尝试镜像自己的 `emacs-nox`，并在低于 29.1 时明确失败；也可提前提供一个与目标 Linux ABI 兼容、目录中含 `bin/emacs` 与 `bin/emacsclient` 的固定 Emacs prefix：

```bash
export MAGENT_BENCH_EMACS_BUNDLE=/absolute/path/to/emacs-29.4-prefix
```

正式跑强制要求这个变量，避免 90 次 trial 因各任务镜像的包仓库而得到不同 Emacs。建议用较老的 glibc 基线构建该 prefix，并先在 smoke 的实际 Harbor task image 中验证动态库兼容性。

同时在 `profiles.toml` 的 `agent_versions` 中填写正式跑使用的 Codex/OpenCode CLI 版本。Magent 的 source tree SHA-256 前缀自动写入 Harbor `agent_info.version`；每个 job 旁还会生成不含 secret 的 fingerprint JSON。

## 分阶段运行

先只生成 YAML，检查 agent、model、dataset 和任务列表：

```bash
uv run magent-bench generate \
  --profile openai-main \
  --model YOUR_OPENAI_MODEL \
  --effort medium \
  --input-price INPUT_USD_PER_MILLION \
  --output-price OUTPUT_USD_PER_MILLION \
  --cached-input-price CACHED_INPUT_USD_PER_MILLION \
  --suite swe-bench-verified \
  --stage smoke

uv run magent-bench generate \
  --profile openai-main \
  --model YOUR_OPENAI_MODEL \
  --effort medium \
  --input-price INPUT_USD_PER_MILLION \
  --output-price OUTPUT_USD_PER_MILLION \
  --cached-input-price CACHED_INPUT_USD_PER_MILLION \
  --suite swe-bench-verified \
  --stage pilot
```

生成物位于 `generated/`。可用 Harbor 执行：

```bash
uv run harbor jobs start --config generated/PROFILE-MODEL-SUITE-STAGE.yaml --yes
```

也可以显式使用 `magent-bench run` 生成并立即执行。只有审完 smoke/pilot 的 ledger、ATIF、失败分类与 verifier 后才执行正式门禁：

```bash
uv run magent-bench run \
  --profile openai-main \
  --model YOUR_OPENAI_MODEL \
  --effort medium \
  --input-price INPUT_USD_PER_MILLION \
  --output-price OUTPUT_USD_PER_MILLION \
  --cached-input-price CACHED_INPUT_USD_PER_MILLION \
  --suite swe-bench-verified \
  --stage full \
  --approve-full
```

对另一个 suite 单独生成/运行。不要把两个 suite 合成一行得分。

## 产物与报告

Magent 每个 trial 会写：

- `instruction.txt`：原始 Harbor instruction；
- `magent-result.json`：终态、耗时和每次 sampling 的 token usage；
- `magent-ledger.json`：Magent canonical thread/turn/item ledger；
- `trajectory.json`：ATIF v1.7；
- `dependencies.json`、`emacs-version.txt` 和 `emacsclient.txt`：运行环境诊断信息。

Codex/OpenCode 的 ATIF 由 Harbor 内置 adapter 生成。聚合报告：

```bash
uv run magent-bench report jobs/your-job --output-dir reports/your-job
```

输出 `trials.csv`、`summary.json`、`report.md`。报告按 suite/model/agent 给出 pass@1、pass@3、resolved rate、token、cost、耗时、tool calls 和失败分类，并对共同实例做 paired bootstrap 95% CI；没有跨 suite composite。

## 本地验证

```bash
make check
```

这只验证免费、确定性的配置/转换/统计逻辑，不会调用模型或启动付费 benchmark。
