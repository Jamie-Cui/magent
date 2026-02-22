# Magent 架构

本文档描述 Magent 的高级架构，Magent 是 OpenCode 的 Emacs Lisp 实现。

## 概述

Magent 是一个多智能体 AI 编程助手，与 LLM 提供商（Anthropic Claude、OpenAI GPT）集成，在 Emacs 中提供智能代码协助。该系统采用模块化架构，在六个主要层之间明确分离关注点。

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
│  - Agent orchestration and loop (max 10 iterations)         │
│  - Tool calling coordination                                │
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
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      API Layer                               │
│                    (magent-api.el)                           │
│  - HTTP client for LLM providers                            │
│  - Message format conversion                                │
│  - Streaming support                                        │
│  - Request/response logging                                 │
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

#### 智能体循环 (`magent-agent.el`)

实现具有工具调用的核心智能体执行循环。

**智能体循环流程：**
1. 接受用户提示
2. 将用户消息添加到会话
3. 使用可用工具调用 LLM API
4. 如果响应包含工具使用：
   - 使用权限检查执行每个工具
   - 将工具结果添加到会话
   - 循环回到步骤 3（最多 10 次迭代）
5. 如果响应仅包含文本：
   - 将助手消息添加到会话
   - 调用完成回调

**主要函数：**
- `magent-agent-process`: 智能体执行的入口点
- `magent-agent--loop`: 主迭代循环
- `magent-agent--get-tools`: 按智能体权限过滤工具
- `magent-agent--execute-tools`: 执行带检查的工具调用

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

每个工具为 LLM 提供一个模式：
- `name`: 工具标识符
- `description`: 工具的功能
- `input_schema`: 参数的 JSON 模式

### 6. API 层 (`magent-api.el`)

与 LLM 提供商通信的 HTTP 客户端。

**支持的提供商：**
- Anthropic Claude（Messages API）
- OpenAI GPT（Chat Completions API）
- OpenAI 兼容 API（自定义基础 URL）

**主要职责：**
- 在内部和提供商特定的消息格式之间转换
- 处理提供商格式中的工具定义
- 解析响应并提取内容/工具使用
- 支持流式响应
- 请求/响应日志记录

**消息转换：**

内部格式使用一致的结构：
```elisp
((role . "user")
 (content . "text"))
```

提供商格式有所不同：
- **Anthropic**: 具有类型的结构化内容块
- **OpenAI**: 简单的字符串内容

**工具使用提取：**

不同的提供商有不同的工具调用格式：
- **Anthropic**: `tool_use` 内容块
- **OpenAI**: 消息中的 `tool_calls` 数组

API 层将这些规范化为内部格式：
```elisp
((id . "call_123")
 (name . "read_file")
 (input . ((path . "/foo/bar.el"))))
```

## 数据流

### 典型请求流程

1. **用户输入**
   - 用户调用 `magent-prompt`（C-c o p）
   - 最小缓冲区提示输入

2. **会话更新**
   - 用户消息添加到会话
   - 会话检索或分配智能体

3. **智能体处理**
   - 智能体循环开始
   - 工具按智能体权限过滤
   - 使用消息 + 工具创建 API 请求

4. **LLM 响应**
   - API 返回响应（文本或工具使用）
   - 响应被解析和规范化

5. **工具执行**（如适用）
   - 每个工具使用都检查权限
   - 如果允许，执行工具
   - 结果添加到会话
   - 循环继续（最多 10 次迭代）

6. **完成**
   - 最终文本响应添加到会话
   - 输出显示给用户
   - 会话被持久化

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

### 4. 最大迭代限制

**决策：** 限制智能体循环为 10 次迭代。

**理由：**
- 防止工具调用中的无限循环
- 控制 API 成本
- 强制更高效的工具使用
- 与 OpenCode 行为相匹配

### 5. 同步工具执行

**决策：** 在智能体循环中同步执行工具。

**理由：**
- 简单性：更容易推理执行顺序
- 可靠性：保证下一次迭代前完成
- Emacs 线程：Emacs 中的异步支持有限
- 未来：如果需要，可以添加异步支持

### 6. 提供商抽象

**决策：** 支持具有统一接口的多个 LLM 提供商。

**理由：**
- 灵活性：用户选择其首选提供商
- 面向未来：易于添加新提供商
- 测试：可以为不同任务切换提供商
- 成本优化：在适当时使用更便宜的模型

## 扩展点

该架构提供了几个扩展点：

1. **自定义智能体**: 添加 `.magent/agent/*.md` 文件
2. **新工具**: 在 `magent-tools.el` 中扩展新功能
3. **新提供商**: 在 `magent-api.el` 中添加提供商支持
4. **权限规则**: 定义自定义权限方案
5. **UI 扩展**: 在 `magent-ui.el` 上构建丰富的显示

## 性能考虑

### 令牌使用

- 会话自动修剪到 `magent-max-history` 消息
- 上下文大小估计有助于监视令牌使用
- 考虑为长对话使用压缩智能体

### API 调用

- 每个智能体循环迭代 = 1 个 API 调用
- 工具密集型任务可能使用多个迭代
- 流式处理减少了感知延迟

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
