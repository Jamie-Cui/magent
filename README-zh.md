# Magent - Emacs AI 编程助手

一个基于 Emacs Lisp 的 AI 编程助手，采用多智能体架构，支持基于权限的工具访问控制，通过 [gptel](https://github.com/karthink/gptel) 集成大语言模型。

## 功能特性

- **多智能体系统**：包含专门的智能体（build、plan、explore、general）
- **基于权限的工具访问**：每个智能体可精细控制工具权限
- **自定义智能体**：通过 `.magent/agent/*.md` 文件创建自定义智能体
- **LLM 集成**：通过 gptel 支持 Anthropic Claude、OpenAI GPT 及兼容 API
- **文件操作**：读取、写入、编辑、grep 搜索、glob 文件匹配
- **Shell 命令执行**：通过 bash 工具执行命令
- **Emacs Lisp 求值**：通过 emacs_eval 工具
- **智能体委托**：explore 和 general 子智能体
- **Claude Code 技能**：与运行中的 Emacs 实例交互
- **会话管理**：对话历史持久化存储
- **Minibuffer 接口**：快速输入提示

## 安装

### 手动安装

将项目添加到 Emacs 加载路径：

```elisp
(add-to-list 'load-path "/path/to/magent/lisp")
(require 'magent)
```

### 使用 `use-package`

```elisp
(use-package magent
  :load-path "/path/to/magent/lisp"
  :config
  (global-magent-mode 1))
```

## 配置

### 快速开始

Magent 将所有 LLM 通信委托给 [gptel](https://github.com/karthink/gptel)。通过 gptel 配置你的提供商、模型和 API 密钥：

```elisp
;; gptel 处理提供商/模型/密钥配置
(setq gptel-model 'claude-sonnet-4-20250514)
(setq gptel-api-key "sk-ant-...")  ; 或使用 ANTHROPIC_API_KEY 环境变量
```

完整提供商配置请参阅 [gptel 文档](https://github.com/karthink/gptel#configuration)（支持 Anthropic、OpenAI、Ollama 等）。

### Magent 专属选项

通过 `M-x customize-group RET magent RET` 自定义：

| 选项 | 默认值 | 描述 |
|------|--------|------|
| `magent-system-prompt` | (内置) | 智能体的默认系统提示 |
| `magent-buffer-name` | `"*magent*"` | 输出缓冲区名称 |
| `magent-auto-scroll` | `t` | 自动滚动输出缓冲区 |
| `magent-enable-streaming` | `t` | 启用流式响应 |
| `magent-enable-tools` | 全部 9 个工具 | 全局启用的工具 |
| `magent-project-root-function` | `nil` | 自定义项目根目录查找函数 |
| `magent-max-history` | `100` | 历史消息最大数量 |
| `magent-default-agent` | `"build"` | 新会话的默认智能体 |
| `magent-load-custom-agents` | `t` | 从 `.magent/agent/*.md` 加载自定义智能体 |
| `magent-enable-logging` | `t` | 启用日志记录到 `*magent-log*` 缓冲区 |
| `magent-assistant-prompt` | `"[AI  ] "` | AI 消息前缀 |
| `magent-user-prompt` | `"[USER] "` | 用户消息前缀 |
| `magent-tool-call-prompt` | `"[TOOL] "` | 工具调用通知前缀 |
| `magent-error-prompt` | `"[ERR ] "` | 错误消息前缀 |
| `magent-agent-directory` | `".magent/agent"` | 自定义智能体目录相对路径 |
| `magent-session-directory` | `~/.emacs.d/magent-sessions/` | 会话持久化目录 |
| `magent-grep-program` | `"rg"` | ripgrep 二进制路径 |

## 使用方法

### 交互式命令

| 命令 | 快捷键 | 描述 |
|------|--------|------|
| `magent-prompt` | `C-c m p` | 输入提示并发送给 AI |
| `magent-prompt-region` | `C-c m r` | 将选中区域发送给 AI |
| `magent-ask-at-point` | `C-c m a` | 询问光标处的符号 |
| `magent-clear-session` | `C-c m c` | 清除当前会话 |
| `magent-show-session` | `C-c m s` | 显示会话摘要 |
| `magent-show-log` | `C-c m l` | 查看 API 请求/响应日志 |
| `magent-clear-log` | `C-c m L` | 清除日志缓冲区 |
| `magent-select-agent` | `C-c m A` | 为当前会话选择智能体 |
| `magent-show-current-agent` | `C-c m i` | 显示当前会话的智能体 |
| `magent-list-agents` | `C-c m v` | 列出所有可用智能体 |

### 快速示例

```elisp
;; 启用模式
(magent-mode 1)

;; 或全局启用
(global-magent-mode 1)

;; 发送提示
M-x magent-prompt

;; 或使用快捷键
C-c m p
```

## 智能体系统

Magent 采用多智能体架构，不同的智能体具有不同的能力和权限。

### 内置智能体

| 智能体 | 模式 | 描述 |
|--------|------|------|
| `build` | primary | 默认智能体，用于通用编程任务，拥有完整工具访问权限 |
| `plan` | primary | 规划智能体，限制文件编辑（仅 `.magent/plan/*.md`） |
| `explore` | subagent | 快速代码库探索（仅 read/grep/glob/bash） |
| `general` | subagent | 通用子智能体（禁止委托） |
| `compaction` | primary（隐藏） | 会话摘要/对话压缩 |
| `title` | primary（隐藏） | 对话标题生成 |
| `summary` | primary（隐藏） | PR 风格摘要生成 |

### 智能体模式

- **primary**：面向用户的智能体，可被选择用于会话
- **subagent**：内部智能体，由主智能体调用执行子任务
- **all**：既可以作为主智能体，也可以作为子智能体

### 权限系统

每个智能体都有权限规则控制工具访问：

```elisp
;; 智能体定义中的权限规则示例
(
 (read_file . allow)           ; 允许所有文件读取
 (write_file . ((deny "*.env")  ; 禁止写入 .env 文件
                (deny "*.key")  ; 禁止写入 .key 文件
                (allow "*")))   ; 允许写入其他文件
 (bash . ask)                  ; 执行 bash 命令前询问用户
)
```

权限操作：
- `allow`：允许使用工具
- `deny`：禁止使用工具
- `ask`：询问用户确认

### 自定义智能体

通过在 `.magent/agent/` 目录添加 markdown 文件创建自定义智能体：

**示例：`.magent/agent/reviewer.md`**

```markdown
---
description: 代码审查专家
mode: primary
hidden: false
temperature: 0.3
permissions:
  - (read_file . allow)
  - (write_file . deny)
  - (bash . deny)
  - (grep . allow)
  - (glob . allow)
---

你是一位代码审查专家。分析代码的以下方面：
- Bug 和潜在问题
- 代码风格和最佳实践
- 性能优化建议
- 安全漏洞

提供具体的建设性反馈和示例。
```

YAML frontmatter 支持：
- `description`：智能体简短描述
- `mode`：`primary`、`subagent` 或 `all`
- `hidden`：从智能体选择界面隐藏
- `temperature`：覆盖默认温度
- `model`：覆盖默认模型
- `permissions`：权限规则列表

## 可用工具

AI 智能体可以访问以下工具（可按智能体自定义）：

| 工具 | 副作用 | 描述 |
|------|--------|------|
| `read_file` | 无 | 从文件系统读取文件内容 |
| `write_file` | 有 | 写入/创建文件（自动创建父目录） |
| `edit_file` | 有 | 精确替换文件中的文本（必须唯一匹配） |
| `grep` | 无 | 通过 ripgrep 进行正则搜索，返回 `file:line:content` |
| `glob` | 无 | 查找匹配 glob 模式的文件 |
| `bash` | 有 | 执行 shell 命令（默认超时 30 秒） |
| `emacs_eval` | 有 | 求值 Emacs Lisp 表达式（默认超时 10 秒） |
| `delegate` | 有 | 使用命名子智能体生成嵌套请求 |
| `skill_invoke` | 无 | 调用 Claude Code 技能（如 Emacs 交互） |

有副作用的工具在执行前会提示用户确认。

工具可用性由以下控制：
1. 全局 `magent-enable-tools` 设置
2. 每个智能体的权限规则

## 架构

### 核心流程

1. **入口点** (`magent.el`)：定义 `magent-mode` 次要模式，使用 `C-c m` 快捷键前缀。初始化智能体注册表、加载自定义智能体和 Claude Code 技能。

2. **智能体系统**：基于权限的工具访问多智能体架构：
   - `magent-agent.el`：构建 gptel 提示，应用每智能体覆盖，调用 `gptel-request`
   - `magent-agent-registry.el`：智能体结构定义、内置智能体定义和中央注册表（整合自原先的独立文件）
   - `magent-agent-file.el`：自定义智能体加载器（`.magent/agent/*.md`）

3. **权限系统** (`magent-permission.el`)：基于规则的每智能体工具访问控制，支持 glob 文件模式匹配。

4. **FSM** (`magent-fsm.el`)：工具调用循环的有限状态机（INIT → SEND → WAIT → PROCESS → TOOL → DONE/ERROR）。当前将 HTTP 委托给 gptel。

5. **LLM 集成**（通过 gptel）：所有 LLM 通信由 [gptel](https://github.com/karthink/gptel) 处理。提供商、模型和 API 密钥配置完全由 gptel 管理。

6. **工具** (`magent-tools.el`)：注册为 `gptel-tool` 结构的工具实现，根据权限规则按智能体过滤。

7. **技能** (`magent-skills.el` + `magent-skill-emacs.el`)：技能注册表，内置 emacs 技能用于 Emacs 交互。技能在进程内直接调用，通过 `skill_invoke` 工具提供。

8. **会话** (`magent-session.el`)：对话历史管理，支持每会话智能体分配和 JSON 持久化。

### 文件结构

```
lisp/
├── magent.el                  # 主入口和模式定义
├── magent-config.el           # 配置（customize group，17 个 defcustom 变量）
├── magent-session.el          # 会话和消息历史（JSON 持久化）
├── magent-tools.el            # 工具实现（9 个 gptel-tool 结构）
├── magent-agent.el            # 智能体逻辑（gptel 集成）
├── magent-agent-registry.el   # 智能体结构、内置定义和注册表
├── magent-agent-file.el       # 自定义智能体文件加载器（.magent/agent/*.md）
├── magent-permission.el       # 权限系统（allow/deny/ask，glob 模式）
├── magent-fsm.el              # 工具调用循环 FSM
├── magent-skill-emacs.el      # 内置 Emacs 交互技能
├── magent-skills.el           # 技能注册表
├── magent-ui.el               # Minibuffer UI 和输出缓冲区渲染
└── magent-pkg.el              # 包描述符
```

### 核心数据结构

- `magent-agent-info`：智能体配置结构（名称、描述、模式、权限、提示、温度、模型等）
- `magent-session`：对话状态，包含消息列表、分配的智能体和历史裁剪
- `magent-fsm`：工具调用循环的 FSM 状态（state、session、agent、tools、pending-tools 等）
- `magent-skill`：已加载的 Claude Code 技能（名称、描述、工具、body、目录）

## 开发

### 构建命令

```bash
make compile    # 字节编译所有 Elisp 文件
make test       # 运行测试（需要 magent-tests.el）
make clean      # 删除编译的 .elc 文件
```

### 单文件编译

```bash
emacs -Q --batch -L lisp -f batch-byte-compile lisp/magent-foo.el
```

## 会话持久化

会话自动保存到 `~/.emacs.d/magent-sessions/`（可配置）。每个会话维护：
- 对话历史
- 分配的智能体
- 消息元数据

## 故障排除

### 启用日志

查看请求/响应日志：

```elisp
(setq magent-enable-logging t)
M-x magent-show-log    ; C-c m l
```

### 检查智能体配置

```elisp
M-x magent-list-agents        ; 列出所有智能体
M-x magent-show-current-agent ; 显示当前会话的智能体
```

### 验证 API 密钥

API 密钥由 gptel 管理。检查方法：

```elisp
gptel-api-key  ; 应返回你的 API 密钥
```

## 许可证

本项目采用 GNU General Public License v3.0 许可 - 详见 [LICENSE](LICENSE) 文件。

## 致谢

- **灵感来源**：[OpenCode](https://github.com/anomalyco/opencode)

## 贡献

欢迎贡献！请确保：
- 代码遵循现有风格约定
- 所有文件字节编译无警告
- 新功能更新文档

## 链接

- **项目仓库**：https://github.com/jamie-cui/magent