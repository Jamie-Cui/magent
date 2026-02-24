# Magent 架构

本文档描述 Magent 的高级架构，Magent 是 OpenCode 的 Emacs Lisp 实现。

## 概述

Magent 是一个多智能体 AI 编程助手，通过 [gptel](https://github.com/karthink/gptel) 与 LLM 提供商集成，在 Emacs 中提供智能代码协助。该系统采用模块化架构，在五个主要层之间明确分离关注点。

## 系统架构

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interface Layer                    │
│                       (magent-ui.el)                         │
│  - Minibuffer prompts                                        │
│  - Output buffer display                                     │
│  - Interactive commands                                      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Session Layer                           │
│                    (magent-session.el)                       │
│  - Conversation history management                           │
│  - Message persistence                                       │
│  - Agent assignment per session                              │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Agent Layer                             │
│           (magent-agent.el, magent-agent-*.el)              │
│  - Agent orchestration via gptel                            │
│  - Per-agent gptel overrides (model, temperature)           │
│  - Permission-based access control                          │
│  - Built-in and custom agent management                     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Permission Layer                        │
│                  (magent-permission.el)                      │
│  - Rule-based tool access control                           │
│  - File pattern matching                                    │
│  - allow/deny/ask resolution                                │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Tools Layer                             │
│                    (magent-tools.el)                         │
│  - read_file, write_file                                    │
│  - grep, glob                                               │
│  - bash command execution                                   │
│  - Registered as gptel-tool structs                         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      LLM Layer (gptel)                       │
│  - HTTP client for LLM providers                            │
│  - Message format conversion                                │
│  - Tool calling FSM (loop management)                       │
│  - Streaming support                                        │
└─────────────────────────────────────────────────────────────┘
```

## 核心组件

### 1. 入口点 (`magent.el`)

主入口点定义 `magent-mode` 次要模式并提供面向用户的界面。

**职责：**
- 模式初始化和按键绑定（`C-c o` 前缀）
- 智能体注册表初始化
- 启动时加载自定义智能体
- 全局模式支持

**主要函数：**
- `magent-mode`: 启用/禁用次要模式
- `global-magent-mode`: 在所有缓冲区中全局启用

### 2. 智能体系统 (Agent System)

智能体系统实现了多智能体架构，其中不同的智能体具有专门的功能和权限规则。

#### 智能体注册表 (`magent-agent-registry.el`)

用于智能体查找和管理的中央注册表。

**职责：**
- 存储和检索智能体定义
- 区分主要智能体和子智能体
- 提供智能体列表和过滤

**主要函数：**
- `magent-agent-registry-register`: 将智能体添加到注册表
- `magent-agent-registry-get`: 按名称检索智能体
- `magent-agent-registry-primary-agents`: 列出面向用户的智能体

#### 智能体信息 (`magent-agent-info.el`)

使用 `cl-defstruct` 定义智能体配置的数据结构。

**结构字段：**
- `name`: 唯一智能体标识符
- `description`: 人类可读的描述
- `mode`: `primary`、`subagent` 或 `all`
- `native`: 是否为内置或自定义
- `hidden`: 隐藏于用户选择
- `prompt`: 智能体的系统提示
- `permission`: 工具访问权限规则
- `temperature`、`top-p`: LLM 采样参数
- `model`: 覆盖默认模型

#### 智能体类型 (`magent-agent-types.el`)

定义七个内置智能体：

1. **build** (primary): 具有完整工具访问权限的默认智能体
2. **plan** (primary): 限制文件编辑的规划模式
3. **explore** (subagent): 快速代码库探索专家
4. **general** (subagent): 多步骤任务执行
5. **compaction** (internal): 会话摘要化
6. **title** (internal): 对话标题生成
7. **summary** (internal): PR 风格摘要

#### 智能体处理 (`magent-agent.el`)

通过 gptel 实现 LLM 通信和工具调用。

**智能体处理流程：**
1. 接受用户提示
2. 将用户消息添加到会话
3. 从会话历史构建 gptel 提示列表
4. 应用每个智能体的 gptel 覆盖（模型、温度、后端）
5. 按智能体权限过滤工具
6. 使用系统提示和可用工具调用 `gptel-request`
7. gptel 的 FSM 在内部管理工具调用循环
8. 回调接收最终字符串响应或错误
9. 将响应记录在会话中

**主要函数：**
- `magent-agent-process`: 智能体执行的入口点
- `magent-agent--make-callback`: 创建处理所有响应类型的 gptel 回调
- `magent-tools-get-gptel-tools`: 按智能体权限过滤工具

#### 自定义智能体 (`magent-agent-file.el`)

从 `.magent/agent/*.md` 文件加载自定义智能体。

**文件格式：**
```markdown
---
description: My custom agent
mode: primary
hidden: false
temperature: 0.5
tools:
  bash: false
  read: true
  write: true
---

Your custom system prompt here.
```

**职责：**
- 解析 YAML 前置元数据
- 转换为智能体信息结构
- 向智能体注册表注册
- 支持将智能体保存回文件

### 3. 权限系统 (`magent-permission.el`)

具有文件模式匹配的基于规则的工具访问控制。

**权限类型：**
- `allow`: 工具被允许
- `deny`: 工具被阻止
- `ask`: 提示用户确认

**规则结构：**
规则是关联列表，可以嵌套以实现细粒度控制：

```elisp
;; Simple tool permission
((bash . deny))

;; File-specific permissions
((read . ((\"*.env\" . deny)
          (\"*.el\" . allow)
          (\"*\" . allow))))

;; Wildcard default
((* . allow))
```

**解析算法：**
1. 检查精确工具匹配
2. 如果工具具有嵌套规则且提供了文件，检查文件模式
3. 检查通配符 `*` 规则
4. 默认为 `allow`

**文件模式匹配：**
- 全局模式: `*.el`、`*.env.*`
- 精确路径: `/path/to/file`
- 通配符默认值: `*`

### 4. 会话管理 (`magent-session.el`)

管理对话状态和历史。

**会话结构：**
- `messages`: 对话消息列表
- `max-history`: 要保留的最大消息数
- `id`: 唯一会话标识符
- `agent`: 当前分配的智能体

**消息类型：**
- `user`: 用户提示
- `assistant`: AI 响应（文本或工具使用）
- `tool`: 工具执行结果

**特性：**
- 自动历史修剪
- 会话持久化到 `~/.emacs.d/magent-sessions/`
- 上下文大小估计（~4 字符每令牌）
- 会话摘要化

### 5. 工具层 (`magent-tools.el`)

实现智能体可以使用的与系统交互的工具。

**可用工具：**

1. **read_file**: 读取文件内容
   - 返回文件文本或错误消息

2. **write_file**: 将内容写入文件
   - 如果需要，创建父目录
   - 返回成功/错误消息

3. **grep**: 使用正则表达式搜索文件
   - 递归目录搜索
   - 跳过 `.git`、`node_modules` 等
   - 返回 `file:line:content` 格式

4. **glob**: 按模式查找文件
   - 支持 `*` 和 `**` 通配符
   - 返回换行符分隔的路径

5. **bash**: 执行 shell 命令
   - 可配置超时（默认 30 秒）
   - 返回 stdout + stderr
   - 超时保护

**工具定义：**

每个工具通过 `gptel-make-tool` 注册为 `gptel-tool` 结构：
- `name`: 工具标识符
- `description`: 工具的功能
- `args`: 参数 plist 列表（name、type、description）
- `function`: 执行的 Emacs Lisp 函数
- `confirm`: 是否需要用户确认（用于 write_file、bash）

### 6. LLM 层 (gptel)

所有 LLM 通信都委托给 [gptel](https://github.com/karthink/gptel)，一个外部 Emacs 包。

**gptel 负责：**
- 与 LLM 提供商的 HTTP 通信（Anthropic、OpenAI、Ollama 等）
- 提供商之间的消息格式转换
- 工具调用 FSM（请求/工具执行/重新请求循环）
- 流式传输支持

**Magent 在 gptel 之上负责：**
- 通过 `magent-agent-info-apply-gptel-overrides` 实现每个智能体的模型/温度覆盖
- 使用基于权限的过滤将工具注册为 `gptel-tool` 结构
- 将会话历史转换为 gptel 提示列表格式
- 回调处理，用于工具调用显示和错误报告

## 数据流

### 典型请求流程

1. **用户输入**
   - 用户调用 `magent-prompt`（C-c o p）
   - 最小缓冲区提示输入

2. **会话更新**
   - 用户消息添加到会话
   - 会话检索或分配智能体

3. **智能体处理**
   - `magent-agent-process` 从会话构建 gptel 提示列表
   - 工具按智能体权限过滤为 `gptel-tool` 列表
   - 应用每个智能体的 gptel 覆盖（模型、温度）
   - 使用系统提示和工具调用 `gptel-request`

4. **gptel 工具调用循环**
   - gptel 向 LLM 提供商发送请求
   - 如果 LLM 返回工具调用，gptel 执行它们并重新请求
   - 此循环完全由 gptel 的 FSM 管理
   - Magent 的回调接收工具调用通知用于 UI 显示

5. **完成**
   - gptel 回调接收最终文本响应
   - 响应添加到会话
   - 输出显示给用户

### 自定义智能体加载流程

1. **模式激活**
   - `magent-mode` 启用
   - 使用内置智能体初始化智能体注册表

2. **自定义智能体发现**
   - 扫描 `.magent/agent/*.md` 文件
   - 解析 YAML 前置元数据
   - 提取智能体配置

3. **智能体注册**
   - 创建 `magent-agent-info` 结构
   - 向智能体注册表注册
   - 可用于会话分配

## 设计决策

### 1. 多智能体架构

**决策：** 支持多个专门的智能体，而不是单个通用智能体。

**理由：**
- 不同的任务受益于不同的权限（例如，规划不需要文件编辑）
- 专门的提示改进了特定于任务的性能
- 用户可以为其工作流创建自定义智能体
- 与 OpenCode 的久经考验的架构相匹配

### 2. 基于权限的访问控制

**决策：** 实现具有文件模式匹配的细粒度权限系统。

**理由：**
- 安全性：防止意外写入敏感文件（`.env`）
- 灵活性：不同的智能体有不同的需求
- 用户控制：`ask` 权限允许用户确认
- 可扩展性：自定义智能体可以指定工具限制

### 3. 基于会话的对话

**决策：** 在会话中维护对话历史，而不是无状态请求。

**理由：**
- 上下文感知：智能体可以参考之前的消息
- 连续性：多轮工具调用需要状态
- 持久性：保存和恢复对话
- 智能体一致性：在整个会话中保持相同的智能体

### 4. 委托给 gptel

**决策：** 使用 gptel 进行所有 LLM 通信，而不是自定义 HTTP 客户端。

**理由：**
- 利用 gptel 成熟的提供商支持（Anthropic、OpenAI、Ollama 等）
- gptel 的 FSM 处理工具调用循环，降低 Magent 的复杂性
- 每个智能体的覆盖允许模型/温度自定义，无需重新实现提供商逻辑
- 用户受益于 gptel 的积极开发和提供商添加

### 5. 同步工具执行

**决策：** 在 gptel 回调中同步执行工具。

**理由：**
- 简单性：更容易推理执行顺序
- 可靠性：保证下一次迭代前完成
- Emacs 线程：Emacs 中的异步支持有限
- 未来：如果需要，可以添加异步支持

## 扩展点

该架构提供了几个扩展点：

1. **自定义智能体**: 添加 `.magent/agent/*.md` 文件
2. **新工具**: 在 `magent-tools.el` 中添加 `gptel-tool` 结构
3. **新提供商**: 通过 gptel 后端配置（无需修改 Magent）
4. **权限规则**: 定义自定义权限方案
5. **UI 扩展**: 在 `magent-ui.el` 上构建丰富的显示

## 性能考虑

### 令牌使用

- 会话自动修剪到 `magent-max-history` 消息
- 上下文大小估计有助于监视令牌使用
- 考虑为长对话使用压缩智能体

### API 调用

- gptel 管理工具调用循环（每次请求多个 API 调用）
- 工具密集型任务会自动使用更多 API 调用
- 通过 gptel 支持流式传输

### 文件操作

- 大文件读取可能会影响性能
- Grep 扫描整个目录树
- Glob 使用 Emacs 内置通配符（高效）

## 未来增强

可能的改进领域：

1. **异步工具执行**: 并行工具调用
2. **工具缓存**: 在会话中缓存文件读取
3. **丰富的 UI**: 具有格式化的专用缓冲区
4. **令牌计数**: 准确的令牌使用跟踪
5. **流式工具结果**: 流式 grep/glob 输出
6. **智能体委派**: 主要智能体调用子智能体
7. **插件系统**: 外部工具定义
