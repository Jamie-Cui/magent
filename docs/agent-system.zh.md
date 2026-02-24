# 智能体系统设计

本文档详细说明了 Magent 中的多智能体架构，包括智能体类型、权限、生命周期和定制化方法。

## 概述

Magent 使用多智能体系统，其中不同的智能体具有专门的能力、提示和工具访问权限。这样可以实现：

- **任务专业化**：不同的智能体针对不同的工作流进行优化
- **安全性**：为每个智能体限制危险操作
- **灵活性**：用户可以为其需求创建自定义智能体
- **效率**：专业化的提示可以减少 token 使用量

## 智能体类型

智能体按照其模式分类，这决定了它们的使用方式：

### 主智能体

用户可交互的智能体，可以为会话选择。

**内置主智能体：**

1. **build**（默认）
   - 通用编码智能体
   - 完整的工具访问权限（read、write、grep、glob、bash）
   - 新会话的默认智能体
   - 温度：0.7

2. **plan**
   - 规划和架构设计
   - 对大多数文件的只读访问
   - 只能编辑 `.magent/plan/*.md` 文件
   - 专注于在实现前组织工作
   - 温度：0.7

### 子智能体

由主智能体调用以执行专门子任务的内部智能体。用户不能直接选择。

**内置子智能体：**

1. **explore**
   - 快速代码库探索
   - 工具：read、grep、glob、bash（只读操作）
   - 针对高效文件搜索的专门化提示
   - 无写入或编辑权限
   - 详尽程度：quick、medium、very thorough

2. **general**
   - 多步骤任务执行
   - 权限与 build 智能体类似
   - 用于委派复杂任务
   - 无法访问 todo 工具（避免循环调用）

### 内部智能体

用于系统操作的隐藏智能体。模式设置为 `all`，但从用户选择中隐藏。

**内置内部智能体：**

1. **compaction**
   - 会话摘要化
   - 无工具访问权限（仅文本生成）
   - 创建详细的会话摘要
   - 当会话变得过长时使用

2. **title**
   - 生成会话标题
   - 无工具访问权限
   - 创建简短标题（≤50 字符）
   - 用于会话管理 UI

3. **summary**
   - PR 风格的摘要
   - 无工具访问权限
   - 2-3 句话的变更摘要
   - 用于提交消息、PR 描述

## 智能体数据结构

智能体使用 `magent-agent-info` struct 定义：

```elisp
(cl-defstruct magent-agent-info
  name           ; Unique identifier (string)
  description    ; Human-readable description
  mode           ; 'primary, 'subagent, or 'all
  native         ; t if built-in, nil if custom
  hidden         ; Hide from user selection
  prompt         ; System prompt (string or nil)
  permission     ; Permission rules (see Permission System)
  temperature    ; Override default temperature
  top-p          ; Override default top-p
  model          ; Override default model
  color)         ; UI color hint
```

## 权限系统

每个智能体都有一套权限规则来控制工具访问。

### 权限操作

- **allow**：无条件允许工具
- **deny**：阻止工具
- **ask**：在执行前提示用户确认

### 权限规则格式

权限定义为具有可选嵌套的 alist：

```elisp
;; 简单规则
((bash . allow)
 (write_file . deny))

;; 特定文件规则
((read_file . ((\"*.env\" . deny)
               (\"*.env.*\" . deny)
               (\"*.env.example\" . allow)
               (\"*\" . allow))))

;; 通配符默认值
((* . allow))
```

### 权限解析

当请求工具时，权限按以下顺序解析：

1. **精确工具匹配**：检查工具特定规则
2. **文件模式匹配**：如果提供了文件，检查模式规则
3. **通配符规则**：检查 `*` 默认值
4. **默认值**：如果没有匹配的规则，返回 `allow`

**解析示例：**

智能体权限规则：
```elisp
((read_file . ((\"*.env\" . deny)
               (\"*\" . allow)))
 (write_file . ask)
 (bash . deny))
```

解析结果：
- `read_file("/foo/bar.el")` → **allow**（匹配 `*` 模式）
- `read_file("/foo/.env")` → **deny**（匹配 `*.env` 模式）
- `write_file("/foo/bar.el")` → **ask**（提示用户）
- `bash("ls")` → **deny**（显式拒绝）
- `grep("pattern", "/")` → **allow**（默认值，无规则指定）

### 默认权限

默认权限集（由 build 智能体使用）：

```elisp
(
 ;; 允许大多数工具
 (* . allow)

 ;; 需要确认的特殊情况
 (doom_loop . ask)
 (external_directory . ask)

 ;; 保护敏感文件
 (read_file . ((\"*.env\" . deny)
               (\"*.env.*\" . deny)
               (\"*.env.example\" . allow)
               (\"*\" . allow)))
)
```

### 智能体权限示例

**Plan 智能体：**
```elisp
(
 (* . allow)  ; 允许大多数工具
 (edit . ((\"*\" . deny)  ; 拒绝所有编辑除了...
          (\".magent/plan/*.md\" . allow)))  ; ...计划文件
)
```

**Explore 智能体：**
```elisp
(
 (* . deny)  ; 默认拒绝
 (grep . allow)
 (glob . allow)
 (list . allow)
 (bash . allow)  ; 只读 bash 命令
 (read . allow)
)
```

**Review 智能体（自定义示例）：**
```elisp
(
 (read_file . allow)
 (grep . allow)
 (glob . allow)
 (write_file . deny)  ; 无修改权限
 (bash . deny)
)
```

## 智能体生命周期

### 1. 注册

智能体在初始化期间被注册：

**内置智能体：**
```elisp
(defun magent-agent-registry-init ()
  "Initialize the agent registry with built-in agents."
  (setq magent--agent-registry (make-hash-table :test 'equal))
  (dolist (agent (magent-agent-types-initialize))
    (magent-agent-registry-register agent)))
```

**自定义智能体：**
```elisp
(when magent-load-custom-agents
  (magent-agent-file-load-all))
```

### 2. 选择

智能体被分配给会话：

**默认分配：**
```elisp
(magent-session-set-agent session
  (magent-agent-registry-get magent-default-agent))
```

**手动选择：**
```elisp
M-x magent-select-agent
```

**程序化：**
```elisp
(magent-agent-process "prompt" #'callback
  (magent-agent-registry-get "explore"))
```

### 3. 执行

智能体通过 gptel 处理用户提示：

1. 获取智能体的系统提示
2. 根据智能体权限将工具过滤为 `gptel-tool` 列表
3. 应用每个智能体的 gptel 覆盖（模型、温度）
4. 调用 `gptel-request` — gptel 管理工具调用循环
5. 回调接收最终响应或错误

### 4. 会话持久化

智能体分配与会话一起持久化：

```elisp
(magent-session-save session)  ; 保存智能体关联
```

## 自定义智能体

用户可以通过将 markdown 文件添加到 `.magent/agent/` 来创建自定义智能体。

### 文件格式

**路径：** `.magent/agent/myagent.md`

```markdown
---
description: Short description of what this agent does
mode: primary
hidden: false
temperature: 0.5
top_p: 0.9
model: claude-sonnet-4-20250514
tools:
  bash: false
  read: true
  write: true
  grep: true
  glob: true
---

You are a specialized agent for [purpose].

Your strengths:
- [Capability 1]
- [Capability 2]

Guidelines:
- [Guideline 1]
- [Guideline 2]

Complete the user's request following these guidelines.
```

### Frontmatter 字段

| 字段 | 类型 | 必需 | 说明 |
|------|------|------|------|
| `description` | string | 是 | 简要的智能体描述 |
| `mode` | string | 否 | `primary`、`subagent` 或 `all`（默认：`all`） |
| `hidden` | boolean | 否 | 从用户选择中隐藏（默认：`false`） |
| `temperature` | float | 否 | 覆盖默认温度（0.0-1.0） |
| `top_p` | float | 否 | 覆盖默认 top-p（0.0-1.0） |
| `model` | string | 否 | 覆盖默认模型 |
| `color` | string | 否 | UI 颜色提示 |
| `tools` | object | 否 | 工具权限（键：工具，值：boolean） |

### 工具配置

`tools` 字段指定启用了哪些工具：

```yaml
tools:
  bash: true       # 允许 bash
  read: true       # 允许 read_file
  write: false     # 拒绝 write_file
  grep: true       # 允许 grep
  glob: true       # 允许 glob
```

**注意：** 如果未提及工具，则默认为 `allow`。要拒绝工具，请显式将其设置为 `false`。

### 自定义智能体示例

#### 代码审查智能体

```markdown
---
description: Code review specialist
mode: primary
hidden: false
temperature: 0.3
tools:
  bash: false
  write: false
---

You are a code review specialist. Analyze code for:
- Bugs and potential issues
- Code style and best practices
- Performance optimizations
- Security vulnerabilities

Provide constructive feedback with specific examples.
Use grep and glob to explore the codebase thoroughly.
```

#### 文档智能体

```markdown
---
description: Documentation writer
mode: primary
temperature: 0.7
tools:
  bash: false
---

You are a documentation specialist. Create clear, comprehensive documentation including:
- API documentation
- User guides
- Code comments
- README files

Focus on clarity and completeness.
Use examples where appropriate.
```

#### 测试生成器智能体

```markdown
---
description: Test case generator
mode: primary
temperature: 0.5
---

You are a test generation specialist. Create comprehensive test suites including:
- Unit tests
- Integration tests
- Edge cases
- Error conditions

Follow the project's testing conventions.
Ensure good code coverage.
```

## 智能体通信

### 主智能体到子智能体

主智能体可以委派任务给子智能体（未来增强）：

```elisp
;; Future API design
(magent-agent-delegate "explore"
  "Find all API endpoint definitions")
```

### 智能体上下文共享

智能体通过会话共享上下文：

- 所有智能体都看到完整的会话历史
- 工具结果对后续迭代可见
- 读取的文件内容是上下文的一部分

## 智能体注册表 API

### 注册

```elisp
(magent-agent-registry-register agent-info)
```

将智能体添加到全局注册表。如果存在同名智能体，则覆盖。

### 查找

```elisp
(magent-agent-registry-get "build")  ; 按名称获取
(magent-agent-registry-get-default)  ; 获取默认智能体
```

### 列表

```elisp
(magent-agent-registry-list)          ; 所有智能体
(magent-agent-registry-primary-agents) ; 仅主智能体
(magent-agent-registry-subagents)     ; 仅子智能体
(magent-agent-registry-list-names)    ; 仅名称
```

### 管理

```elisp
(magent-agent-registry-remove "myagent")  ; 删除智能体
(magent-agent-registry-clear)             ; 清除所有
(magent-agent-registry-init)              ; 重新初始化
```

## 最佳实践

### 创建自定义智能体

1. **清晰的目标**：给智能体一个具体、专注的目标
2. **最少权限**：仅授予必要的工具访问权限
3. **详细的提示**：提供清晰的指导和示例
4. **适当的温度**：确定性任务温度低，创意任务温度高
5. **充分测试**：用各种输入验证智能体行为

### 使用智能体

1. **明智选择**：为任务选择合适的智能体
2. **了解限制**：知道每个智能体可以做什么和不能做什么
3. **会话范围**：在一个会话中保持相关工作
4. **监控迭代**：观察过度的 API 调用
5. **自定义胜过通用**：为重复任务创建专业化智能体

### 权限设计

1. **最少权限原则**：从最少权限开始
2. **文件模式**：使用模式保护敏感文件
3. **危险操作需确认**：为可能有害的操作使用 `ask`
4. **测试模式**：验证文件模式按预期匹配
5. **文档规则**：对复杂权限逻辑进行注释

## 故障排查

### 智能体未找到

```elisp
(magent-agent-registry-list-names)  ; 检查可用智能体
(magent-load-agent-files)           ; 重新加载自定义智能体
```

### 权限被拒绝

检查智能体权限：
```elisp
(let ((agent (magent-agent-registry-get "myagent")))
  (magent-agent-info-permission agent))
```

测试权限解析：
```elisp
(magent-permission-resolve rules 'write_file "/path/to/file")
```

### 自定义智能体未加载

1. 检查文件位置：`.magent/agent/*.md`
2. 验证 frontmatter 中的 YAML 语法
3. 检查 `magent-load-custom-agents` 是否为 `t`
4. 查看 `*Messages*` 缓冲区中的错误
5. 尝试手动加载：`M-x magent-load-agent-files`

### 智能体使用错误的模型

优先顺序：
1. 智能体特定的模型覆盖
2. gptel 默认值（`gptel-model`）

检查智能体的模型覆盖：
```elisp
(magent-agent-info-model agent-info)
```
