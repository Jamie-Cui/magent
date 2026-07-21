# Magent coding-agent benchmark

日常使用只需要看三个文件：

- `config.toml`：本地配置，包含 provider、model、suite、stage、价格和 API key；
- `config.example.toml`：可提交的无密钥模板；
- `Makefile`：`make` 准备，`make bench` 执行。

评测基于 Harbor 0.20.0，分别报告 SWE-bench Verified 和 Terminal-Bench
2.1；不会生成跨 suite 综合分。

## 两步运行

首次使用先创建权限为 `0600` 的本地配置：

```bash
cd benchmark
install -m 600 config.example.toml config.toml
```

然后编辑 `config.toml` 顶部的 `[benchmark]`，并只在所选 profile 中填写
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

该代理只用于本地构建 Emacs bundle，不会传给模型请求或写入 fingerprint。

运行：

```bash
make
make bench
```

例如 DeepSeek 兼容榜的 `[benchmark]` 为：

```toml
[benchmark]
profile = "deepseek-compatibility"
model = "deepseek-v4-pro"
suite = "terminal-bench-2.1"
stage = "smoke"
```

`make` 会自动：

1. 安装固定的 Python/Harbor 依赖；
2. 运行 Python 与 Elisp 的免费确定性测试；
3. 创建或复用固定的本地 ELPA bundle；
4. 首次构建基于 Ubuntu 22.04 的 Emacs 29.4 bundle，之后复用；
5. 检查 model、本地配置中的 API key、Docker、manifest、full-run 门禁和 Harbor schema；
6. 生成 `generated/benchmark.yaml` 及 fingerprint。

`make bench` 只运行已经准备好的配置。若 `config.toml` 在 prepare 后发生变化，
它会要求重新运行 `make`。成功后自动生成报告，无需再调用 report 命令。
Harbor 的动态 trial progress 默认关闭，避免它与 worker 日志在同一 TTY 行互相覆盖；
warning、最终结果表和报告路径仍会正常输出。

`config.toml` 已被 Git 忽略，模板里的 `api_key` 必须始终留空。生成的
`generated/benchmark.yaml` 也被忽略并设为 `0600`，因为它包含传给 Harbor
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

Harbor 原始结果写入 `jobs/`，聚合报告自动写入 `reports/`。报告包含：

- pass@1、pass@3、resolved rate；
- token、成本、耗时和 tool calls；
- 失败分类；
- paired bootstrap 95% CI。

Magent trial 还会保存 `magent-ledger.json`、`magent-result.json` 和 ATIF
`trajectory.json`。

高级调试时仍可运行 `uv run magent-bench --help`，但正常 benchmark 不需要直接使用这些子命令。
