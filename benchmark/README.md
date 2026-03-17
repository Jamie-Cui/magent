# Benchmark

这个目录包含 Magent progressive disclosure 的 benchmark runner、starter suite、fixture 和结果约定。

## 目标

- 比较 `control` 与 `treatment` 两个 arm：
  - `control`：关闭 `magent-enable-capabilities`
  - `treatment`：开启 capability progressive disclosure
- 在相同模型、相同 prompt、相同初始状态下，对比：
  - `primary_success`
  - `task_success`
  - `quality_score`
  - `input/output/total tokens`（后端提供时）
  - `request_bytes`（始终可用的成本代理）
  - `tool_call_count`
  - `approval_request_count`
  - capability 激活命中、漏激活和误激活

## 文件结构

- `magent-benchmark.el`
  核心 runner、原始结果写入、汇总分析
- `magent-benchmark-suite.el`
  starter suite 任务定义
- `run-benchmark.el`
  batch 入口
- `fixtures/`
  benchmark 固定输入

## Starter Suite

当前 suite 按 4 类任务组织：

- `capability-match`
  明确应激活 capability，且多数任务要求读取 live Emacs state
- `capability-ambiguous`
  prompt 本身较弱，更依赖 buffer/file 上下文
- `capability-negative`
  不应激活 capability 的普通文件任务
- `potential-misfire`
  prompt 含 `commit`、`status` 等容易误触发的词，但实际不该走 Magit workflow

其中 Org 和 runtime 任务会对 visiting buffer 做未保存修改，目的是让 benchmark 能区分：

- 只读磁盘文件
- 真正检查 live Emacs state

## 运行方式

推荐在已经配置好 gptel backend/model 的 live Emacs session 中运行。

### live Emacs

```sh
emacsclient --eval '
(progn
  (load "/home/jamie/opt/emacs.d/site-lisp/magent/benchmark/magent-benchmark.el" nil t)
  (magent-benchmark-run-suite :repetitions 2))'
```

### batch

```sh
emacs -Q --script benchmark/run-benchmark.el -- --repetitions 2
```

可选参数：

- `--repetitions N`
- `--output-dir DIR`
- `--task-regexp REGEXP`

## 输出

默认输出目录为 `benchmark/results/<timestamp>/`，包含：

- `results-raw.jsonl`
  每个 `task x arm x repetition` 一条记录
- `summary.json`
  聚合统计
- `summary.md`
  便于人工查看的摘要
- `manifest.json`
  本次运行的任务和参数

## 说明

- token 使用量取决于 gptel backend 是否返回 usage 字段。
- 当后端没有返回完整 token usage 时，summary 仍会保留：
  - `output_tokens`（若可得）
  - `request_bytes`
- `request_bytes` 不是 token，但可以作为稳定的成本代理，用来做前后对比。
