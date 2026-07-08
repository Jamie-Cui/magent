---
title: Agent 工作流状态机
lang: zh
alt_url: /AGENT_WORKFLOW.html
---

# Agent 工作流状态机

Magent 现在把 agent loop 建模为显式的 `thread -> turn -> item` ledger。这个 ledger 是 agent workflow state 的 source of truth。旧 session `messages` 和 `context-items` 仍然保留，但只作为 gptel prompt construction、迁移和旧测试的 derived projections。

## Codex 对齐

Codex 在 SDK/app-server 边界暴露 thread event stream：

- `thread.started`
- `turn.queued`
- `turn.started`
- `item.started`
- `item.updated`
- `item.completed`
- `turn.completed` 或 `turn.failed`

Magent 保留同类工作流边界，但适配 Emacs：

- Provider transport 仍然走 `gptel-request`。
- Magent loop 自己负责 tool dispatch、continuation outcome、abort 和 Emacs UI rendering；turn layer 负责 final-response policy，包括 post-tool continuation 产生空 assistant text 时的一次 no-tool retry。
- 不引入 Codex sandbox、seatbelt、bubblewrap 或 shell isolation parity。

Codex 中最接近的参考点是 SDK event names、core sampling loop、app-server event conversion 和 thread status shape。Magent 借鉴的是事件边界和 turn/item 生命周期，不复制 Codex 的隔离和 app-server runtime。

## 状态对象

`magent-ledger.el` 定义 canonical ledger：

- `magent-thread`
  - 状态：`not-loaded`、`idle`、`active`、`system-error`、`closed`
- `magent-thread-turn`
  - 状态：`queued`、`in-progress`、`completed`、`interrupted`、`failed`、`dropped`
- `magent-thread-item`
  - 状态：`pending`、`in-progress`、`completed`、`failed`、`cancelled`

每个用户 prompt 创建一个 turn。Assistant message、reasoning block、tool invocation 和 tool output 都是这个 turn 下的 item。Reasoning item 永远不会提升成 assistant message text，即使 provider 没有返回可见 content。

| 对象 | 运行态 | 终态 |
| --- | --- | --- |
| thread | `not-loaded`, `idle`, `active`, `system-error`, `closed` | `closed` |
| turn | `queued`, `in-progress` | `completed`, `interrupted`, `failed`, `dropped` |
| item | `pending`, `in-progress` | `completed`, `failed`, `cancelled` |

Turn lifecycle：

```text
queued -> in-progress -> completed
                      -> interrupted
                      -> failed
                      -> dropped
```

Item lifecycle：

```text
pending -> in-progress -> completed
                       -> failed
                       -> cancelled
```

## Tool Items

Tool call 和 tool result 是同一个 item lifecycle，不是两条持久记录。

模型请求工具时，item 开始：

```elisp
(:type tool :status in-progress :name "grep" :input (:pattern "..."))
```

工具结果可用后，同一个 item 完成或失败：

```elisp
(:type tool :status completed :output "...")
```

这和旧的 `tool-call` + `tool-output` 分裂不同。为了 prompt reuse，Magent 仍会把 completed tool item 投影成 gptel historical `(tool . PLIST)` 形状。

如果模型请求工具而结果稍后到达，Magent 按 `call-id` 更新同一个 item。如果旧路径只记录了 tool result，loop 会创建 synthetic turn，让 item 仍然有 durable turn parent。

## 持久化

持久化格式是 `snapshot + journal`。

- `journal`：append-only event log，记录 `turn-queued`、`turn-started`、`item-started`、`item-completed`、`turn-completed` 等生命周期事件。它是审计记录，即使事件已经反映在 snapshot 中也会保留。
- `snapshot`：materialized full thread state。它保存当前 thread、turns 和 items，避免每次 resume 都从头 replay。

Session JSON 同时写入两者：

```json
{
  "snapshot": { "id": "...", "turns": [] },
  "journal": [{ "type": "turn-started", "...": "..." }]
}
```

旧 `messages` 和 `context-items` 是 prompt reuse 和迁移用的 projections。加载时，Magent 优先 replay `snapshot + journal`；若不存在则把 legacy `messages` 迁移成 thread ledger。

Replay 语义：

1. 把 `snapshot` 载入 materialized thread state。
2. 附加全部 `journal` events，保留 audit/history 可见性。
3. 只应用 `seq > snapshot.last-event-seq` 的事件。

因此 `snapshot` 是快速恢复点，`journal` 是 append-only record，两者不可互换。

## Loop Flow

1. UI submission 进入 `magent-runtime-api.el`。
2. `magent-runtime-api.el` 创建 queued ledger turn，并立即记录 completed user message item。
3. submission 真正开始时，`magent-runtime-api.el` 把 turn 转成 `in-progress`。
4. `magent-agent-process` 幂等复用该 turn/user item，不重复写用户消息。
5. `magent-agent-loop` 消费 normalized LLM events。
6. Text 和 reasoning deltas 更新 snapshot 中 materialized in-progress items，但不会为每个 chunk 追加 journal event；terminal item event 携带最终内容。
7. Tool-call events 累积到 provider-neutral `tool-call-batch-end`。
8. Tool dispatch 启动 `tool` items，记录 approval metadata，并把同一 item 更新为 `completed` 或 `failed`。
9. Tool output 返回 `tool-output` 等 continuation outcome；`magent-agent-process` 决定是否从 session history 重建 prompt 并继续 sampling。
10. 如果 post-tool continuation 返回空 assistant text，`magent-agent-process` 会先尝试一次 no-tool final-response request，再决定是否完成 turn。Sampling-limit finalization 使用同样的 no-tool request 形状，但有独立 metadata。
11. Assistant completion 记录 assistant message item，并完成 turn。
12. Abort、failure 和 dropped queued submissions 分别把 turn 转成 `interrupted`、`failed` 或 `dropped`；aborted turn 下的 in-progress items 标记为 `cancelled`。
13. `magent-max-sampling-requests` 是单个用户 turn 的 lifecycle guard。它限制包括 tool-output continuations 在内的 model sampling request 数量，但不检查或去重 tool command 内容。

Prompt reconstruction 是 ledger-driven。知道 current turn id 时，Magent 只包含 completed history 和该 turn，避免后续 queued user submissions 泄漏进 active model request。

## UI 投影

默认 UI backend 是 agent-shell。`magent-ui.el` 是 thin command router；普通 prompt 路由到 `magent-agent-shell.el`，后者创建 `magent-acp.el` 实现的 in-process ACP client。ACP session/prompt request 提交到 `magent-runtime-api.el`，并保持 pending 直到对应 runtime turn completed、failed 或 cancelled。Runtime 发出 Magent-native observer events；`magent-acp.el` 转换为 ACP `session/update` 消息。

`magent-runtime-queue.el` 负责 queued/active turn state。当前实现一次只有一个全局 active turn，但 submission 都带 runtime session id，所以 cancellation 是 session-scoped：取消一个 ACP session 会移除该 session 的 queued work 并 abort 该 session 的 active turn，不会丢掉其他 session 的 queued turns。

Legacy workspace/compose UI 隔离在 `magent-ui-legacy.el`。它派生自 `special-mode`，显示 oldest-first timeline，并用独立的 `magent-compose-mode` buffer 记录 scope-specific prompt text。Reasoning 存为 ledger items；legacy workspace 只显示状态和字符数，完整内容可从 transcript/detail 命令查看。

## 保留的 Codex 差异

Magent 刻意保留以下差异：

- UI 是 Emacs-native 且 backendized：默认 agent-shell + in-process ACP，旧 workspace/compose UI 是 legacy backend。
- Provider streaming 经 `magent-llm-gptel.el` 规范化，但 transport 仍是 gptel。
- Codex core 有 app-server mailbox、steering queue 和更丰富的 intra-turn input 语义；Magent 把 request serialization 放在 runtime queue，用 Codex-style tool continuation 处理工具结果，并在 post-tool response 为空时只追加一次 no-tool final answer 请求。
- Magent 用自己的 session JSON `snapshot + journal`，不直接使用 Codex rollout files。
- Tool call 和 output 在 Magent 中是一个 source-of-truth item 的状态更新。
- Tool execution 当前由 Magent tool queue 串行化；Codex 更丰富的 per-tool runtime、approval、MCP 和 process execution 机制不在当前范围。
- Child agents 是 `AGENT_JOBS.md` 中描述的 durable Magent jobs，不是 Codex app-server threads。

## Backlog / TODO

Agent-workflow/UI refactor backlog 已无开放 TODO。未来 hardening 候选：

- 为更多结构化输出增加 per-tool renderer plugins。
- 增加超大 transcript 和长 streaming session 的 live visual smoke coverage。

## 设计评审摘要

当前主 agent-loop 相比 Codex 的主要差距已经不再是缺少显式 thread/turn/item lifecycle；Magent 已经拥有并持久化该 lifecycle。

剩余差异是有意的产品/运行时边界：

- 不提供 Codex app-server lifecycle manager、subscriber model、unloaded thread cache 或 active flags。
- 不复制 Codex 的 intra-turn input queue/mailbox semantics。
- 不直接使用 Codex rollout files。
- 不把 tool call 和 tool output 分成两条 source-of-truth records。
