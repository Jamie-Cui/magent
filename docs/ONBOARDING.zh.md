---
title: Magent 入门导览
lang: zh
alt_url: /ONBOARDING.html
---

# Magent 入门导览

**更新日期：** 2026-07-07

## 项目概览

**Magent** 是一个 Emacs Lisp AI coding agent，包含多 agent 架构、durable turn ledger 和 permission-based tool access。

- **语言：** Emacs Lisp
- **主要依赖：** [gptel](https://github.com/karthink/gptel)，负责全部 LLM 通信
- **要求：** Emacs 29.1+，gptel，transient，compat，yaml，acp，agent-shell，ripgrep
- **目标：** 在 live Emacs runtime 内提供 AI-assisted coding，同时保持 provider transport 在 gptel 内，并让工具访问显式、可审计

## 当前 Agent 工作流

Magent 拥有 Magent-owned agent loop 和 durable child-agent lifecycle。Root agent 可以 spawn、message、wait、list、inspect/resume 和 close child-agent jobs。

修改 child-agent behavior、session persistence 或 agent-related tools 之前，先读 `docs/AGENT_JOBS.md`。Codex sandbox behavior 明确不在范围内。

## 架构分层

### 第 1 层：入口与配置

这一层初始化系统并管理设置。

**关键文件：**

- `magent.el`：主入口，定义带 `C-c m` 前缀的 `magent-mode` minor mode。
- `magent-config.el`：全部 `defcustom`、`defface` 和共享工具。
- `magent-pkg.el`：package metadata。

**职责：** `magent--ensure-initialized` 在第一次命令调用时触发 lazy initialization。启用 mode 只添加 modeline construct，agent registry、skills 等完整初始化按需执行。

### 第 2 层：Session 与 Runtime State

这一层管理 conversation history、scoped overlays 和 runtime state。

**关键文件：**

- `magent-thread.el`：thread/turn/item ledger、状态转移和 snapshot shape。
- `magent-turn.el`：turn 创建、queue/start/finish state transitions 和 dropped-turn handling。
- `magent-session.el`：conversation projections、JSON persistence、per-project sessions。
- `magent-agent-job.el`：durable child-agent job records 和 JSON shape。
- `magent-runtime.el`：静态初始化，以及 agents、skills、capabilities 的 project-local overlay activation。
- `magent-runtime-api.el`：UI/backend-facing runtime session 和 prompt API。
- `magent-runtime-queue.el`：全局单执行 runtime queue 和 session-scoped cancellation。
- `magent-audit.el`：permission 和 sensitive action 的持久 JSONL audit logging。

**职责：** 维护 project-scoped、ledger-backed conversation history，默认持久化到 `~/.emacs.d/magent/sessions/`，在 `agent-jobs` 下保存 durable child-agent jobs，并在 scope 变化时加载或卸载 project-local overlays。

### 第 3 层：Agent System

多 agent 架构，为不同任务提供专门 agent。

**关键文件：**

- `magent-agent.el`：核心 agent processing，构造 gptel prompt、应用 overrides、调用 `gptel-request`。
- `magent-agent-registry.el`：agent struct、7 个内置 agents、hash-table registry。
- `magent-agent-file.el`：从 `.magent/agent/*.md` 加载 custom agents。
- `magent-permission.el`：rule-based tool access control，支持 allow/deny/ask 和 glob patterns。

**职责：** 提供 `build`、`plan`、`explore`、`general` 等 agent。Permission system 按 agent 过滤工具。Custom agents 通过带 YAML frontmatter 的 markdown 文件扩展功能。

**当前行为：** 旧的一次性 `delegate` surface 已替换成 durable child-agent jobs，拥有稳定 id、status、transcript/result storage 和 parent/child session relationship。

### 第 4 层：Tools 与 Capabilities

这一层执行 agent 请求的具体动作。

**关键文件：**

- `magent-tools.el`：14 个 `gptel-tool` structs，包括 file I/O、grep/glob、bash、emacs_eval、child-agent coordination、skill_invoke 和 web_search。
- `magent-skills.el`：skill registry、built-in skills、file loading 和 inspection commands。
- `magent-capability.el`：capability definitions、resolution 和 file-backed loading。
- `magent-approval.el`：敏感操作的 user approval prompts。

**职责：** Tools 提供具体动作；skills 扩展 agent 行为；capability resolver 根据上下文自动激活少量 instruction skills；approval system gate dangerous operations。

### 第 5 层：Agent Loop 与 LLM Integration

这一层编排 tool-calling loop 和 LLM communication。

**关键文件：**

- `magent-agent-loop.el`：Magent-owned loop、tool dispatch、serial queueing、abort helpers、continuation。
- `magent-llm.el`：provider-neutral request/event protocol。
- `magent-llm-gptel.el`：薄的 `gptel-request` sampling adapter。

**职责：** `magent-agent-loop.el` 消费 normalized LLM events，记录 assistant/tool state，经过 `magent-tool-orchestrator` 分发工具，处理 visible tool rendering、abort cleanup 和 Codex-style continuation。`magent-llm-gptel.el` 仍然调用 `gptel-request`；Magent 不重写 provider transport。

### 第 6 层：UI Backends

默认 agent-shell UI，加上隔离的 legacy workspace/compose backend。

**关键文件：**

- `magent-ui.el`：thin public command router、logger、backend switch 和 compatibility shims。
- `magent-agent-shell.el`：Magent agent-shell config 和 prompt routing。
- `magent-acp.el`：in-process ACP adapter。
- `magent-runtime-api.el`：UI/backend-facing runtime API。
- `magent-runtime-queue.el`：runtime queue 和 session-scoped cancellation。
- `magent-ui-legacy.el`：legacy workspace/compose buffers、ledger projection、streaming summaries、transient menu。
- `magent-evil.el`：Evil 用户显式加载的可选 integration。
- `magent-md2org.el`：legacy markdown -> org converter，不再用于 live workspace path。
- `magent-file-loader.el`：agent/skill/capability 共享 frontmatter parser。

**职责：** 普通 prompt 默认走 agent-shell 和进程内 ACP adapter。ACP session/prompt request 提交到 `magent-runtime-api.el`，直到对应 Magent turn 完成才返回。Legacy UI 仍可用，但已隔离在 `magent-ui-legacy.el`。

### 第 7 层：Events

**关键文件：**

- `magent-events.el`：turn、subagent 和 tool call 的 structured lifecycle hooks。

## 核心概念

### Multi-Agent Architecture

内置 agents：

- **build**：默认 agent，用于 general coding，工具访问最完整。
- **plan**：规划 agent，限制文件编辑范围。
- **explore**：快速代码库探索，只读/搜索/有限 bash。
- **general**：child-agent task 的 general-purpose subagent。
- **compaction**、**title**、**summary**：隐藏 utility agents。

Agent modes：

- `primary`：用户可选的 session agent。
- `subagent`：内部子任务 agent。
- `all`：可作为 primary 或 subagent。

### Permission System

每个 agent 有细粒度工具规则：

```elisp
((read_file . allow)
 (write_file . ((deny "*.env")
                (deny "*.key")
                (allow "*")))
 (bash . ask))
```

解析顺序：exact tool match -> file-pattern rules -> wildcard (`*`) -> default allow。

### Skill System

Skill 有两类：

- **instruction**：Markdown 注入 system prompt。
- **tool**：通过 `skill_invoke` tool 调用。

Instruction skills 可以作为下一次请求的一次性上下文。Agent-shell backend 提供 `magent-agent-shell-toggle-skill-for-next-request` 和 `magent-agent-shell-run-skill-command` 等命令；legacy transient Skills submenu 保留同类 workflow。带 `default-prompt` 的 skills 也可作为 command-like actions 运行，例如 built-in `init` 用于初始化或刷新项目根目录 `AGENTS.md`。Compose buffers 是普通 prompt text，不再解析 `@skill` 或 `@clear` 控制语法。

Skills 加载顺序：内置 `skills/`，用户 `~/.emacs.d/magent-skills/`，项目 `.magent/skills/`。

### Session Scoping

Sessions 是 project-aware：

- 在 project 内：state scoped to that project。
- 在 project 外：使用 global session fallback。
- 默认保存到 `~/.emacs.d/magent/sessions/`。
- `agent-jobs` 保存 durable child-agent job metadata、result/error state 和 transcripts。

### Lazy Initialization

启用 mode 很轻量，只更新 modeline。完整初始化在第一次命令通过 `magent--ensure-initialized` 触发。

## 代码阅读路线

### Step 0：先读架构边界

先读 `docs/ARCHITECTURE.md`，理解 Magent 的产品边界：provider plumbing 留在 gptel，agent loop 运行在 Emacs Lisp 中，通过 tools 暴露 Emacs runtime context，不实现 Codex-style OS sandbox。

### Step 1：入口

从 `magent.el` 开始，理解 mode 如何启用以及公开命令有哪些。`C-c m` prefix map 是主要 interactive entry points。

### Step 2：UI 边界

读 `docs/UI_BACKENDS.md`，再读 `magent-ui.el`、`magent-agent-shell.el` 和 `magent-acp.el`。关键点：`magent-ui.el` 是 thin router；默认交互体验是 agent-shell；legacy workspace rendering 和 compose submission 在 `magent-ui-legacy.el`。

### Step 3：请求生命周期

按顺序跟踪一次请求：

1. `magent-agent.el`：`magent-agent-process` 构造 prompt。
2. `magent-agent-loop.el`：normalized events、tool dispatch、queueing、abort 和 continuation。
3. `magent-llm-gptel.el`：为一次 sampling 调用 `gptel-request`。
4. `magent-tool-orchestrator.el` / `magent-tools.el`：解析权限并执行 tool implementations。
5. `magent-runtime-api.el` / `magent-acp.el`：backend submissions 和 agent-shell 的 UI-neutral events。
6. `magent-ui-legacy.el`：legacy backend 激活时的 buffer rendering。

### Step 4：权限

读 `magent-permission.el`，看 `magent-permission-resolve` 的解析顺序。再读 `magent-agent-registry.el`，理解内置 agents 如何定义 permissions。

### Step 5：扩展点

关注：

- `magent-agent-file.el`：custom agents 如何从 `.magent/agent/*.md` 加载。
- `magent-skills.el`：skills 如何扩展 agent 能力。
- `magent-file-loader.el`：agent、skill、capability 共用的 frontmatter parser。

### Step 6：工具

读 `magent-tools.el`，重点看：

- `emacs_eval`：在 request buffer context 中执行。
- `spawn_agent` / `send_agent_message` / `wait_agent` / `list_agents` / `close_agent`：durable child-agent jobs。
- `web_search`：通过 `url-retrieve` 和 DuckDuckGo 实现。

然后读 `docs/AGENT_JOBS.md`，掌握生命周期 contract 和 persistence boundaries。

### Step 7：测试

看 `test/magent-test.el`。测试用 `cl-letf` mock `gptel-request` 和 UI functions，并绑定 registry/session 到 fresh state。这是理解 public API surface 的好入口。

## 复杂热点

### 1. `magent-agent-loop.el`

**复杂原因：** 它拥有 active request/tool loop：normalized event accumulation、tool-call batch completion、serial execution、permission orchestration、UI tool rendering、abort cleanup、tool-result session recording 和 continuation。

**修改建议：** 任何 loop state、tool callback ordering 或 abort handling 的改动都可能导致 turn hang 或 session history 损坏。先加 ERT，再在 Emacs 可用时用 tool-use prompts 做 live 验证。

### 2. `magent-llm-gptel.el`

**复杂原因：** 它是唯一允许触碰 gptel callback/FSM 细节的位置。

**修改建议：** provider transport concern 留在这里，loop behavior 留在 `magent-agent-loop.el`。不要让主 loop 依赖 gptel private FSM handlers。

### 3. UI backend boundary

**复杂原因：** 默认 UI 横跨 `magent-ui.el`、`magent-agent-shell.el`、`magent-acp.el`、`magent-runtime-api.el` 和 `magent-runtime-queue.el`。Legacy workspace/compose 仍有自定义 insertion、async fontification、chunk batching 和 read-only region management。

**修改建议：** 默认 agent-shell 行为不要放进 `magent-ui-legacy.el`。Backend-neutral 行为放 `magent-runtime-api.el`，ACP conversion 放 `magent-acp.el`，agent-shell-specific 行为放 `magent-agent-shell.el`。

### 4. `magent-permission.el`

**复杂原因：** file-pattern matching 是 order-dependent，解析顺序是 exact match -> file patterns -> wildcard -> default allow。

**修改建议：** 更具体的 pattern 放在更不具体的前面，并为不同 glob pattern 加测试。

### 5. `magent-tools.el`

**复杂原因：** 14 个 tool implementation 各自有 side effects、timeout、child-agent runtime state 和 error handling。`emacs_eval` 要在正确 buffer context 中执行。

**修改建议：** timeout handling 必须可靠；`emacs_eval` context capture、child-agent status persistence 和 parent/child request-context inheritance 都很关键。

### 6. `magent-session.el`

**复杂原因：** 它负责 project scoping、global fallback、JSON persistence、ledger-driven UI restoration、legacy buffer-content migration、child-agent job persistence 和 history trimming。

**修改建议：** session directory 由 project root SHA1 派生。`buffer-content` 和 `agent-jobs` 需要为恢复和 child transcript inspection 保持无损。

## 开发流程

### 构建与测试

```bash
make compile
make test
make test-unit
make coverage
make clean
```

`make test-live-smoke` 需要 Emacs server，并使用 stubbed gptel transport。`make test-live` 使用真实 provider，可能消耗 tokens，应在 isolated daemon 中运行。

### Live Development

```bash
emacsclient --eval '(load "/path/to/magent-foo.el" nil t)'
emacsclient --eval '(magent-clear-session)'
emacsclient --eval '(with-current-buffer "*magent-log*" (buffer-string))'
```

### Test Prompts

修改后用这些 prompt 验证：

- **Non-tool:** `"你好"`，验证 streaming 和 assistant sections。
- **Tool-use:** `"帮我看下 emacs 里面有多少 buffer"`，验证 `emacs_eval` tool。
- **Multi-step:** `"帮我在 emacs 里面打开 magent 的 magit buffer"`，验证 chained execution。

检查 `*magent*`、`*magent-log*` 和 `*Messages*` 是否有错误。

## 常见模式

### 新增 Tool

1. 在 `magent-tools.el` 定义：

   ```elisp
   (defun magent-tools--my-tool (args)
     ;; Implementation
     )

   (defvar magent-tools--my-tool-tool
     (gptel-make-tool :function #'magent-tools--my-tool
                      :name "my_tool"
                      :description "What it does"))
   ```

2. 加入 `magent-config.el` 的 `magent-enable-tools` default。
3. 更新 `magent-agent-registry.el` 中的 agent permissions。

如果是 `spawn_agent`、`send_agent_message`、`wait_agent`、`list_agents` 或 `close_agent` 这类 agent lifecycle tools，要同步更新 `docs/AGENT_JOBS.md` 和相关测试。

### 创建 Custom Agent

创建 `.magent/agent/my-agent.md`：

```markdown
---
description: Agent purpose
mode: primary
temperature: 0.7
permissions:
  - (read_file . allow)
  - (write_file . ask)
  - (bash . deny)
---

System prompt goes here.
```

### 新增 Skill

创建 `skills/my-skill/SKILL.md`：

```markdown
---
name: my-skill
description: Brief description
type: instruction
tools: read_file, grep
---

Skill instructions for the agent.
```

## 获取帮助

- **Documentation:** `README.org` 和仓库根目录 `AGENTS.md`
- **Child-agent architecture:** `docs/AGENT_JOBS.md`
- **Interactive help:** `M-x magent-doctor`
- **Command menu:** `C-c m ?`，或在 `*magent*` 中按 `?`
- **Skills:** transient `s s` 选择 one-shot instruction skill，`s K` 清空
- **Logs:** transient `l l`
- **Agent info:** transient `A v` 列出 agents，`A m` 查看 current agent

## 下一步

1. 先运行 `make test-unit`；跑完整 `make test` 前准备 isolated Emacs server。
2. 启用 `magent-mode`，试运行 `C-c m p`。
3. 按上面的 guided tour 读代码。
4. 试着创建一个 custom agent 或 skill。
5. 查看 open issues 并提交 PR。
