# Magent - Emacs AI 编程助手

一个基于 Emacs Lisp 的 AI 编程助手，采用多智能体架构，支持基于权限的工具访问控制，通过 [gptel](https://github.com/karthink/gptel) 集成大语言模型。

## 功能特性

- **多智能体系统**：包含专门的智能体（build、plan、explore、general）
- **基于权限的工具访问**：每个智能体可精细控制工具权限
- **自定义智能体**：通过 `.magent/agent/*.md` 文件创建自定义智能体
- **LLM 集成**：通过 gptel 支持 Anthropic Claude、OpenAI GPT 及兼容 API
- **文件操作**：读取、写入、grep 搜索、glob 文件匹配
- **Shell 命令执行**：通过 bash 工具执行命令
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
| `magent-enable-tools` | `(read write grep glob bash)` | 启用的工具 |
| `magent-max-history` | `100` | 历史消息最大数量 |
| `magent-default-agent` | `"build"` | 新会话的默认智能体 |
| `magent-load-custom-agents` | `t` | 从 `.magent/agent/*.md` 加载自定义智能体 |
| `magent-enable-logging` | `t` | 启用日志记录到 `*magent-log*` 缓冲区 |

## 使用方法

### 交互式命令

| 命令 | 快捷键 | 描述 |
|------|--------|------|
| `magent-prompt` | `C-c o p` | 输入提示并发送给 AI |
| `magent-prompt-region` | `C-c o r` | 将选中区域发送给 AI |
| `magent-ask-at-point` | `C-c o a` | 询问光标处的符号 |
| `magent-clear-session` | `C-c o c` | 清除当前会话 |
| `magent-show-session` | `C-c o s` | 显示会话摘要 |
| `magent-view-log` | `C-c o l` | 查看 API 请求/响应日志 |
| `magent-clear-log` | `C-c o L` | 清除日志缓冲区 |
| `magent-select-agent` | `C-c o A` | 为当前会话选择智能体 |
| `magent-show-current-agent` | `C-c o i` | 显示当前会话的智能体 |
| `magent-list-agents` | `C-c o v` | 列出所有可用智能体 |

### 快速示例

```elisp
;; 启用模式
(magent-mode 1)

;; 或全局启用
(global-magent-mode 1)

;; 发送提示
M-x magent-prompt

;; 或使用快捷键
C-c o p
```

## 智能体系统

Magent 采用多智能体架构，不同的智能体具有不同的能力和权限。

### 内置智能体

| 智能体 | 模式 | 描述 |
|--------|------|------|
| `build` | primary | 默认智能体，用于通用编程任务，拥有完整工具访问权限 |
| `plan` | primary | 规划智能体，用于设计和架构任务 |
| `explore` | subagent | 快速探索智能体，用于代码库导航 |
| `general` | subagent | 通用子智能体，用于委托任务 |
| `compaction` | all | 对话压缩智能体 |
| `title` | all | 会话标题生成智能体 |
| `summary` | all | 摘要智能体 |

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

| 工具 | 描述 |
|------|------|
| `read_file` | 从文件系统读取文件内容 |
| `write_file` | 写入内容到文件 |
| `grep` | 使用正则表达式在文件中搜索 |
| `glob` | 查找匹配 glob 模式的文件 |
| `bash` | 执行 shell 命令 |

工具可用性由以下控制：
1. 全局 `magent-enable-tools` 设置
2. 每个智能体的权限规则

## 架构

### 核心流程

1. **入口点** (`magent.el`)：定义 `magent-mode` 次要模式及快捷键。初始化智能体注册表并加载自定义智能体。

2. **智能体系统**：基于权限的工具访问多智能体架构：
   - `magent-agent.el`：构建 gptel 提示，应用每智能体覆盖，调用 `gptel-request`
   - `magent-agent-registry.el`：智能体查找/注册中心
   - `magent-agent-info.el`：智能体配置数据结构
   - `magent-agent-types.el`：内置智能体定义
   - `magent-agent-file.el`：自定义智能体加载器

3. **权限系统** (`magent-permission.el`)：基于规则的每智能体工具访问控制，支持文件模式匹配。

4. **LLM 集成**（通过 gptel）：所有 LLM 通信、消息格式化和工具调用循环由 [gptel](https://github.com/karthink/gptel) 处理。

5. **工具** (`magent-tools.el`)：注册为 `gptel-tool` 结构的工具实现，根据权限规则按智能体过滤。

6. **会话** (`magent-session.el`)：对话历史管理，支持每会话智能体分配和持久化。

### 文件结构

```
lisp/
├── magent.el                  # 主入口和模式定义
├── magent-config.el           # 配置（customize group）
├── magent-session.el          # 会话和消息历史
├── magent-tools.el            # 工具实现（gptel-tool 结构）
├── magent-agent.el            # 智能体逻辑（gptel 集成）
├── magent-agent-registry.el   # 智能体注册系统
├── magent-agent-info.el       # 智能体数据结构
├── magent-agent-types.el      # 内置智能体定义
├── magent-agent-file.el       # 自定义智能体文件加载器
├── magent-permission.el       # 权限系统
├── magent-ui.el               # Minibuffer UI 和显示
└── magent-pkg.el              # 包描述符
```

### 核心数据结构

- `magent-agent-info`：智能体配置结构，包含名称、模式、权限、提示
- `magent-session`：对话状态，包含消息列表和分配的智能体
- `magent-permission`：基于规则的访问控制，支持文件模式匹配

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
M-x magent-view-log
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