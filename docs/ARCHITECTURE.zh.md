---
title: Magent 架构
lang: zh
alt_url: /ARCHITECTURE.html
---

# Magent 架构

Magent 是一个 Emacs-native 的 AI coding agent。它的核心设计判断是：重度 Emacs 工作流里已经有大量活状态，包括 buffer、mode、project、Magit、Org、xref、LSP、process buffer，以及用户长期积累的交互习惯。因此 Magent 不是把终端 agent 包进 Emacs，而是在 Emacs Lisp 里实现 agent runtime。

系统边界刻意保持清晰：

- Provider transport、HTTP/SSE、模型选择和 API key 继续交给 `gptel`，通过 `gptel-request` 调用。
- 用户交互默认走 `agent-shell`，中间由进程内 ACP adapter 连接；旧的 special-mode workspace 保留为隔离的 legacy backend。
- Agent 执行、工具编排、权限决策、会话、技能、能力解析和 child-agent jobs 由 Magent 自己管理。
- Codex 风格的 seatbelt、bubblewrap、sandbox 和 shell isolation 不在范围内。Magent 的 permission 是工作流确认和审计，不是 OS 安全边界。

## 系统边界

```text
User
  |
  v
agent-shell / legacy UI
  |
  v
magent-ui.el -> magent-agent-shell.el -> magent-acp.el
  |
  v
magent-runtime-api.el -> magent-runtime-queue.el
  |
  v
magent-thread.el / magent-session.el 的 thread/turn/item ledger
  |
  v
magent-agent.el -> magent-agent-loop.el
  |                     |
  |                     v
  |              magent-tool-orchestrator.el -> magent-tools.el
  v
magent-llm-gptel.el -> gptel-request -> provider
```

这个结构让替换点保持可见。换 provider 应该局限在 `magent-llm-gptel.el` 和 gptel 配置里；换 UI 应该局限在 `magent-ui.el`、`magent-acp.el` 和 `magent-runtime-api.el` 附近；持久化和恢复应该通过 ledger/session 模块；工具策略应该通过 permission 和 orchestrator 模块。

## 依赖层次

### Emacs Runtime

Emacs 不只是宿主进程。Magent 依赖活的编辑器状态：buffer、major mode、project root、timer、process、URL、JSON、`special-mode` 和用户安装的包。`emacs_eval` 很关键，因为它让 agent 能检查当前编辑器，而不是只能读磁盘文件。

### Provider Plumbing

`magent-llm.el` 定义 provider-neutral request/event。`magent-llm-gptel.el` 负责一次 sampling request：调用 `gptel-request`，再把 gptel callback 转成 Magent normalized events。这个 adapter 可以隐藏 gptel callback/FSM 细节，但主 loop 只消费 normalized events。

### UI 与 Runtime API

默认 UI backend 是 `agent-shell`。`magent-agent-shell.el` 注册 Magent 的 agent-shell 配置，`magent-acp.el` 实现进程内 ACP adapter，`magent-runtime-api.el` 接收 UI backend 的 prompt submission。`magent-runtime-queue.el` 当前采用全局单执行模型，同时保留 session-scoped cancellation 和队列状态。

旧 workspace/compose UI 位于 `magent-ui-legacy.el`。它按需加载，不应该成为默认 agent-shell 路径的依赖。

### Ledger 与持久化

持久化真相源是 `thread -> turn -> item` ledger。`magent-thread.el` 定义状态对象和转移，`magent-turn.el` 创建和推进 turn，`magent-session.el` 保存 materialized `snapshot` 和 append-only `journal`。

旧的 `messages`、`context-items` 和 `buffer-content` 只是 projection 或迁移数据，不是 UI 的 canonical source of truth。

### 工具、权限与审计

`magent-tools.el` 暴露 14 个 `gptel-tool`：

- `read_file`、`write_file`、`edit_file`
- `grep`、`glob`、`bash`
- `emacs_eval`
- `spawn_agent`、`send_agent_message`、`wait_agent`、`list_agents`、`close_agent`
- `skill_invoke`、`web_search`

`magent-tool-orchestrator.el` 负责权限解析、必要时请求 approval、执行工具、写 audit，再把结果交回 loop。`magent-permission.el` 的解析顺序是：精确 tool 规则、file-pattern 规则、wildcard fallback，最后 default allow。

## 请求生命周期

1. 用户 prompt 从 agent-shell 或 legacy UI 进入。
2. UI backend 提交到 `magent-runtime-api.el`。
3. `magent-turn` 记录 queued turn 和 completed user item。
4. `magent-runtime-queue.el` 在全局执行槽空闲时启动 turn。
5. `magent-agent.el` 选择 session agent、active skills、capability instructions 和允许的工具。
6. `magent-llm-gptel.el` 调用 `gptel-request` 发起一次 sampling。
7. `magent-agent-loop.el` 接收 normalized text、reasoning、tool-call 和 completion events。
8. tool call 累积到 `tool-call-batch-end` 后，通过 orchestrator 串行执行。
9. tool result 更新同一个 tool item，而不是创建一条独立的持久记录。
10. 如果 tool output 需要返回给模型，`magent-agent.el` 从 ledger 重建 prompt 并开始下一次 sampling。
11. completion、failure、abort 或 queued drop 会把 turn 转到 terminal state，并通知 UI backend。

这个 continuation 模型接近 Codex：工具结果会触发后续采样；但实现仍然是 Emacs-native 且由 gptel 承担 provider transport。

## 扩展模型

Magent 有三层 file-backed 扩展：

- `.magent/agent/*.md` 下的 custom agents。
- bundled `skills/`、用户 `~/.emacs.d/magent-skills/`、项目 `.magent/skills/` 下的 skills。
- bundled `capabilities/` 和项目 `.magent/capabilities/` 下的 capabilities。

Instruction skill 会注入 system prompt。Tool skill 通过 `skill_invoke` 调用。Capability 会根据当前上下文打分，激活少量 instruction skills。这种 progressive disclosure 能保持基础 prompt 更小，同时在需要时注入 Emacs-specific workflow 知识。

## 产品取舍

Magent 更接近 “gptel + stateful agent runtime”，不是 gptel 的替代品。相比在 Emacs 里包一层终端 agent，Magent 能检查和利用活的编辑器上下文。相比 Codex 或 Claude Code，Magent 不追求多端覆盖和强 OS 隔离；它的优势是低摩擦地进入长期使用的 Emacs 环境。

实际开发时应保留这些边界：

- Provider 工作留在 gptel 和 `magent-llm-gptel.el`。
- 默认 UI 工作不要放进 `magent-ui-legacy.el`。
- durable workflow state 留在 ledger。
- child-agent 行为保持和 `docs/AGENT_JOBS.md` 一致。
- 把 permission 视为确认/审计工作流，而不是 sandbox 安全。

## 来源说明

本概览整理自 `/home/jamie/opt/org-root/roam/2026-03-24t1544.org` 中的项目架构笔记，并已按当前 checkout 更新。
