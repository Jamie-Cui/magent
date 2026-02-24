# LLM 集成指南

本文档描述了 Magent 如何通过 [gptel](https://github.com/karthink/gptel) 与 LLM 提供商集成。

## 概述

Magent 将所有 LLM 通信委托给 gptel，一个用于与大型语言模型交互的 Emacs 包。gptel 处理提供商特定的 API、消息格式化、工具调用循环和流式传输。Magent 在 gptel 之上构建，添加了每个智能体的配置覆盖、基于权限的工具过滤和会话管理。

## 支持的提供商

gptel 支持的任何提供商都可以与 Magent 配合使用。常见选项：

### Anthropic Claude

**环境变量：** `ANTHROPIC_API_KEY`

**模型：**
- `claude-opus-4-20250514`（最强大）
- `claude-sonnet-4-20250514`（平衡、默认）
- `claude-haiku-4-20250514`（最快）

### OpenAI

**环境变量：** `OPENAI_API_KEY`

**模型：**
- `gpt-4-turbo`
- `gpt-4`
- `gpt-3.5-turbo`

### 其他提供商

gptel 支持许多其他提供商，包括 Ollama（本地模型）、Azure OpenAI、Google Gemini 等。请参阅 [gptel 的 README](https://github.com/karthink/gptel#supported-llm-backends) 获取完整列表。

## 配置

### 基本设置

所有提供商、模型和 API 密钥配置都通过 gptel 完成：

```elisp
;; Anthropic（默认 gptel 后端）
(setq gptel-api-key "sk-ant-...")
(setq gptel-model 'claude-sonnet-4-20250514)

;; 或使用环境变量
;; export ANTHROPIC_API_KEY="sk-ant-..."
```

对于非默认提供商，配置 gptel 后端：

```elisp
;; OpenAI
(setq gptel-backend
      (gptel-make-openai "OpenAI"
        :key "sk-..."
        :models '(gpt-4-turbo gpt-4 gpt-3.5-turbo)))

;; 通过 Ollama 使用本地模型
(setq gptel-backend
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :models '(llama3 codellama)))
```

请参阅 [gptel 配置文档](https://github.com/karthink/gptel#configuration) 获取完整的设置说明。

### 每个智能体覆盖

各个智能体可以覆盖默认的模型和温度：

```elisp
;; 在自定义智能体文件中 (.magent/agent/myagent.md)
---
model: claude-haiku-4-20250514
temperature: 0.3
---
```

或以编程方式：

```elisp
(setf (magent-agent-info-model agent-info) "claude-haiku-4-20250514")
(setf (magent-agent-info-temperature agent-info) 0.3)
```

这些覆盖通过 `magent-agent-info-apply-gptel-overrides` 在请求期间临时应用，该函数在 `gptel-request` 调用期间设置相关的 gptel 变量。

## Magent 如何使用 gptel

### 请求流程

```
magent-agent-process
    |
从会话构建提示列表 (magent-session-to-gptel-prompt-list)
    |
从智能体获取系统提示 (或 magent-system-prompt)
    |
按智能体权限过滤工具 (magent-tools-get-gptel-tools)
    |
应用每个智能体的覆盖 (magent-agent-info-apply-gptel-overrides)
    |
gptel-request (提示列表, :system, :stream nil, :callback)
    |
gptel 处理 LLM 通信和工具调用循环
    |
回调接收：字符串响应、工具结果或错误
```

### 工具注册

工具使用 `gptel-make-tool` 注册为 `gptel-tool` 结构：

```elisp
(gptel-make-tool
 :name "read_file"
 :description "Read the contents of a file at the given path."
 :args (list '(:name "path"
               :type string
               :description "Absolute or relative path to the file"))
 :function #'magent-tools--read-file
 :category "magent")
```

修改系统的工具（`write_file`、`bash`）包含 `:confirm t`，以便 gptel 可以提示用户确认。

### 工具过滤

并非所有工具都对所有智能体可用。`magent-tools-get-gptel-tools` 通过以下方式过滤全局工具列表：

1. **全局配置**：仅考虑 `magent-enable-tools` 中的工具
2. **智能体权限**：仅考虑智能体权限规则允许的工具

### 回调处理

`magent-agent--make-callback` 创建一个处理以下情况的 gptel 回调：

- **字符串响应**：最终文本 -- 添加到会话，传递给 UI
- **工具结果**：工具已执行，gptel 继续循环 -- 在 UI 中显示
- **工具调用待处理**：等待用户确认 -- 记录
- **中止/错误**：请求失败 -- 显示为错误

## 日志记录

### 启用日志

```elisp
(setq magent-enable-logging t)
```

### 查看日志

```elisp
M-x magent-view-log
```

### 清除日志

```elisp
M-x magent-clear-log
```

Magent 将智能体活动记录到 `*magent-log*` 缓冲区。要进行更底层的 HTTP 调试，启用 Emacs URL 库调试模式：

```elisp
(setq url-debug t)
```

## 成本优化

### 令牌用量

通过会话上下文大小监控大致令牌用量：

```elisp
(magent-session-get-context-size session)
;; => 大致令牌计数
```

### 模型选择

为每个智能体选择合适的模型：
- **简单任务**：使用更快/更便宜的模型（Haiku、GPT-3.5）
- **复杂任务**：使用更强大的模型（Opus、GPT-4）
- **代码生成**：平衡模型效果很好（Sonnet、GPT-4-turbo）

在自定义智能体文件中覆盖模型：

```yaml
---
model: claude-haiku-4-20250514
---
```

## 故障排查

### 找不到 API 密钥

确保 gptel 有正确的 API 密钥：

```elisp
gptel-api-key  ; 检查当前密钥
(setq gptel-api-key "your-key")
;; 或设置 ANTHROPIC_API_KEY / OPENAI_API_KEY 环境变量
```

### 错误的模型或提供商

检查 gptel 配置：

```elisp
gptel-model    ; 当前模型
gptel-backend  ; 当前后端/提供商
```

### 智能体使用错误的模型

每个智能体的模型覆盖优先。检查智能体的模型字段：

```elisp
(magent-agent-info-model (magent-agent-registry-get "myagent"))
```

优先顺序：
1. 智能体特定的模型覆盖
2. gptel 默认值（`gptel-model`）

### 连接问题

要进行 HTTP 级别调试：

```elisp
(setq url-debug t)
```

检查 gptel 自身的故障排查：`M-x gptel` 应该独立于 Magent 工作。
