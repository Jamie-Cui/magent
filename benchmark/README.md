# Magent coding-agent benchmark

日常使用只需要看三个文件：

- `config.toml`：本地配置，包含 provider、model、suite、stage、价格和 API key；
- `config.example.toml`：可提交的无密钥模板；
- 项目根目录的 `Makefile`：`make benchmark` 自动准备并执行。

评测基于 Harbor 0.20.0，分别报告 SWE-bench Verified 和 Terminal-Bench
2.1；不会生成跨 suite 综合分。

## 运行

首次使用先创建权限为 `0600` 的本地配置：

```bash
install -m 600 benchmark/config.example.toml benchmark/config.toml
```

然后编辑 `benchmark/config.toml` 顶部的 `[benchmark]`，并只在所选 profile 中填写
`api_key`：

```toml
[profiles.deepseek-compatibility]
api_key = "你的密钥"
```

每个 agent 的名称、模型模板和可选 CLI 版本写在同一行。要做单 agent
调试，直接注释掉对应整行即可，不需要同步修改其他表。例如：

```toml
agents = [
  # { name = "opencode", model = "deepseek/{model}", version = "" },
  { name = "magent", model = "{model}" },
]
```

`version = ""` 表示使用最新外部 CLI；Magent 直接记录当前源码 digest，
不需要 `version`。OpenAI 主榜的正式运行仍要求保留三个 agent。

如果本机代理监听 `10808`，可以在同一个本地配置的 `[benchmark]` 中设置：

```toml
proxy = "http://127.0.0.1:10808"
```

该代理同时用于本地构建 Emacs bundle、Harbor task environment 的 Docker build
和 trial 容器运行期，包括 agent 安装、工具联网和模型请求。loopback 地址会在容器中改写为
`host.docker.internal`；宿主机代理必须监听 Docker 可访问的接口。代理值不会写入
fingerprint，但会和 API key 一样出现在权限为 `0600` 的私有 generated job 中。
启用代理时，agent 安装超时会从 Harbor 默认的 6 分钟放宽为 12 分钟。

运行：

```bash
make benchmark
```

例如 DeepSeek 兼容榜的 `[benchmark]` 为：

```toml
[benchmark]
profile = "deepseek-compatibility"
model = "deepseek-v4-pro"
suite = "terminal-bench-2.1"
stage = "smoke"
```

若尚未准备或 `config.toml` 已更新，`make benchmark` 会自动：

1. 安装固定的 Python/Harbor 依赖；
2. 运行 Python 与 Elisp 的免费确定性测试；
3. 创建或复用固定的本地 ELPA bundle；
4. 首次构建基于 Ubuntu 22.04 的 Emacs 29.4 bundle，之后复用；
5. 检查 model、本地配置中的 API key、Docker、manifest、full-run 门禁和 Harbor schema；
6. 生成 `benchmark/generated/benchmark.yaml` 及 fingerprint。

准备成功后，同一次 `make benchmark` 会立即开始可能付费的评测。成功后自动生成报告，
无需再调用 report 命令。若只想完成免费检查和准备，运行
`make benchmark-prepare`；若只想运行已有准备结果，运行 `make benchmark-run`。
wrapper 会显示一条 Overall progress，并为 Codex、OpenCode、Magent 中实际参与本次
job 的每个 agent 各显示一条 progress；每行同时显示 completed/total、running、
errors、累计 input/cache/output token，以及仍在运行时距最近可观测事件的时间。
OpenCode 和 Magent 会在 model step 结束时更新 live token；`seen...` 持续增长
只表示暂时没有新的磁盘事件，不会单独判定 trial 失败。Harbor 的单 trial progress
会隐藏，避免多 worker 输出互相覆盖。
Public-network task 会忽略 agent provider allowlist，因此 wrapper 只过滤 Harbor
为此逐 trial 重复产生的良性 warning；其他 warning、最终结果表和报告路径仍会正常输出。

项目根目录的 `make clean` 会停止并删除名称匹配 Harbor trial 约定的 benchmark
容器和网络，同时删除 Elisp 编译产物及本地可重建环境。`make purge` 还会删除
benchmark Docker 镜像和 `~/.cache/harbor` task cache，之后重跑需要重新下载和构建。
两个命令都保留 `benchmark/config.toml`、`benchmark/jobs/` 和
`benchmark/reports/`，也不会调用可能影响其他项目的全局 Docker prune。

`config.toml` 已被 Git 忽略，模板里的 `api_key` 必须始终留空。生成的
`benchmark/generated/benchmark.yaml` 也被忽略并设为 `0600`，因为它包含传给 Harbor
容器的运行时密钥；不要复制或发布该文件。`api_key_env` 只是容器内部接收
密钥的变量名，不再从宿主机环境变量读取。

## Stage

| Stage | 任务 | 每题次数 | OpenAI 主榜 trials |
|---|---:|---:|---:|
| `smoke` | 1 | 1 | 3 |
| `pilot` | 10 | 1 | 30 |
| `full` | 30 | 3 | 270 |

OpenAI 主榜同时运行 Codex、OpenCode 和 Magent。DeepSeek 兼容榜默认运行
OpenCode 和 Magent，本地调试时可以按上面的方式注释一行只运行 Magent；
不会为 Codex 静默切换模型或 provider。

`full` 仍然保留必要的可复现性门禁，全部写在同一个 `config.toml`：

- `approve_full = true`；
- 输入、输出 token 价格；
- Codex/OpenCode 固定版本；
- `elpa_bundle` 与 `emacs_bundle`。

这些门禁不会为了缩短命令而被隐藏或跳过。

## 结果

Harbor 原始结果写入 `benchmark/jobs/`，聚合报告自动写入 `benchmark/reports/`。报告包含：

- pass@1、pass@3、resolved rate；
- token、成本、耗时和 tool calls；
- 失败分类；
- paired bootstrap 95% CI。

Magent trial 还会保存 `magent-ledger.json`、`magent-result.json` 和 ATIF
`trajectory.json`。

高级调试时仍可在项目根目录运行
`uv --directory benchmark run --locked magent-bench --help`。例如可直接调用
`profiles` 或 `prepare-emacs` 子命令，但正常 benchmark 不需要这些 Make 包装目标。
