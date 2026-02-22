# API 集成指南

本文档描述了 Magent 如何与大型语言模型 (LLM) 提供商集成，包括 Anthropic Claude、OpenAI GPT 和与 OpenAI 兼容的 API。

## 概述

API 层 (`magent-api.el`) 为多个 LLM 提供商提供统一接口，同时处理提供商特定的消息格式、工具定义和响应解析。

## 支持的提供商

### Anthropic Claude

**提供商 ID：** `'anthropic`

**API 端点：** `https://api.anthropic.com/v1/messages`

**身份验证：** 通过 `x-api-key` 请求头的 API 密钥

**环境变量：** `ANTHROPIC_API_KEY`

**模型：**
- `claude-opus-4-20250514` (最强大)
- `claude-sonnet-4-20250514` (平衡、默认)
- `claude-haiku-4-20250514` (最快)

### OpenAI

**提供商 ID：** `'openai`

**API 端点：** `https://api.openai.com/v1/chat/completions`

**身份验证：** 通过 `Authorization` 请求头的承载令牌 (Bearer token)

**环境变量：** `OPENAI_API_KEY`

**模型：**
- `gpt-4-turbo`
- `gpt-4`
- `gpt-3.5-turbo`

### OpenAI 兼容

**提供商 ID：** `'openai-compatible`

**API 端点：** 通过 `magent-base-url` 配置

**身份验证：** 通过 `Authorization` 请求头的承载令牌

**使用场景：**
- 本地 LLM (LM Studio、Ollama with OpenAI compatibility)
- 替代提供商 (Azure OpenAI 等)
- 自托管模型

## 配置

### 基本设置

```elisp
;; Anthropic
(setq magent-provider 'anthropic)
(setq magent-api-key "sk-ant-...")
(setq magent-model "claude-sonnet-4-20250514")

;; OpenAI
(setq magent-provider 'openai)
(setq magent-api-key "sk-...")
(setq magent-model "gpt-4-turbo")

;; OpenAI 兼容
(setq magent-provider 'openai-compatible)
(setq magent-base-url "http://localhost:1234/v1")
(setq magent-api-key "not-needed-for-local")
(setq magent-model "local-model-name")
```

### 环境变量

除了直接设置 `magent-api-key`，还可以使用环境变量：

```bash
# 在你的 shell 配置文件中
export ANTHROPIC_API_KEY="sk-ant-..."
export OPENAI_API_KEY="sk-..."
```

Magent 会自动检测并使用这些变量。

### 验证

检查凭证是否已配置：

```elisp
M-x magent-api-set-credentials
```

或以编程方式：

```elisp
(magent-get-api-key)  ; 返回密钥或 nil
```

## 消息格式

### 内部格式

Magent 使用一致的内部消息格式：

```elisp
;; 简单文本消息
((role . "user")
 (content . "Hello, world!"))

;; 结构化内容 (Anthropic 风格)
((role . "assistant")
 (content . (((type . "text")
              (text . "Response text"))
             ((type . "tool_use")
              (id . "call_123")
              (name . "read_file")
              (input . ((path . "/foo/bar.el")))))))

;; 工具结果
((role . "tool")
 (content . ((type . "tool_result")
             (tool_use_id . "call_123")
             (content . "File contents..."))))
```

### 提供商特定转换

#### Anthropic Messages API

**请求格式：**
```json
{
  "model": "claude-sonnet-4-20250514",
  "max_tokens": 8192,
  "temperature": 0.7,
  "messages": [
    {
      "role": "user",
      "content": "Hello"
    }
  ],
  "tools": [
    {
      "name": "read_file",
      "description": "Read file contents",
      "input_schema": {
        "type": "object",
        "properties": {
          "path": {
            "type": "string",
            "description": "File path"
          }
        },
        "required": ["path"]
      }
    }
  ]
}
```

**响应格式：**
```json
{
  "id": "msg_123",
  "type": "message",
  "role": "assistant",
  "content": [
    {
      "type": "text",
      "text": "I'll read that file."
    },
    {
      "type": "tool_use",
      "id": "call_123",
      "name": "read_file",
      "input": {
        "path": "/foo/bar.el"
      }
    }
  ]
}
```

#### OpenAI Chat Completions API

**请求格式：**
```json
{
  "model": "gpt-4-turbo",
  "max_tokens": 8192,
  "temperature": 0.7,
  "messages": [
    {
      "role": "user",
      "content": "Hello"
    }
  ],
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "read_file",
        "description": "Read file contents",
        "parameters": {
          "type": "object",
          "properties": {
            "path": {
              "type": "string",
              "description": "File path"
            }
          },
          "required": ["path"]
        }
      }
    }
  ]
}
```

**响应格式：**
```json
{
  "id": "chatcmpl-123",
  "object": "chat.completion",
  "choices": [
    {
      "message": {
        "role": "assistant",
        "content": "I'll read that file.",
        "tool_calls": [
          {
            "id": "call_123",
            "type": "function",
            "function": {
              "name": "read_file",
              "arguments": "{\"path\": \"/foo/bar.el\"}"
            }
          }
        ]
      }
    }
  ]
}
```

## 工具定义

### 内部工具模式 (Schema)

工具使用以下结构定义：

```elisp
((name . "read_file")
 (description . "Read the contents of a file")
 (input_schema
   (type . "object")
   (properties . ((path . ((type . "string")
                           (description . "File path")))))
   (required . (path))
   (additionalProperties . :json-false)))
```

### 提供商转换

#### Anthropic

直接使用 `input_schema` (原生格式)。

#### OpenAI

转换为函数调用格式：

```elisp
(defun magent-api--convert-tools (tools)
  "Convert internal tool definitions to OpenAI format."
  (mapcar (lambda (tool)
            `((type . "function")
              (function . ((name . ,(cdr (assq 'name tool)))
                          (description . ,(cdr (assq 'description tool)))
                          (parameters . ,(cdr (assq 'input_schema tool)))))))
          tools))
```

## API 调用

### 聊天完成

主要 API 函数：

```elisp
(cl-defun magent-api-chat (messages
                           &key tools stream model
                                max-tokens temperature
                                callback)
  "Send chat completion request to LLM provider.")
```

**参数：**
- `messages`: 消息对象列表
- `tools`: 可用工具定义列表 (可选)
- `stream`: 启用流式传输 (未完全实现)
- `model`: 覆盖默认模型
- `max-tokens`: 最大响应令牌数
- `temperature`: 采样温度 (0.0-1.0)
- `callback`: 用响应调用的函数

**示例：**

```elisp
(magent-api-chat
 '(((role . "user") (content . "Hello")))
 :tools (magent-tools-get-definitions)
 :callback (lambda (response)
             (message "Got: %s"
                     (magent-api--extract-content response))))
```

### 响应处理

响应被解析和规范化：

```elisp
;; 提取文本内容
(magent-api--extract-content response)
;; => "Response text"

;; 提取工具使用
(magent-api--extract-tool-uses response)
;; => (((id . "call_123")
;;      (name . "read_file")
;;      (input . ((path . "/foo/bar.el")))))
```

### 错误处理

检测并抛出 API 错误：

```elisp
(when (assq 'error response)
  (error "API error: %s" (cdr (assq 'error response))))
```

常见错误场景：
- 无效的 API 密钥 → 身份验证错误
- 速率限制 → 429 状态码
- 找不到模型 → 无效模型错误
- 格式错误的请求 → 400 Bad Request

## 日志记录

### 启用日志

```elisp
(setq magent-enable-logging t)
```

### 查看日志

```elisp
M-x magent-view-log
```

### 日志内容

日志包括：
- 请求时间戳
- API 端点
- 消息计数
- 请求体 (JSON)
- 响应体 (JSON)
- 错误消息

**示例日志：**

```
[14:32:15] [API] Sending request to: https://api.anthropic.com/v1/messages
[14:32:15] [API] Messages count: 3
[14:32:15] [API] Request body: {"model":"claude-sonnet-4-20250514",...}
[14:32:17] [API] Response received: {"id":"msg_123",...}
```

### 清除日志

```elisp
M-x magent-clear-log
```

## 流式传输支持

### 当前状态

基本的流式基础设施存在，但未完全与代理循环集成。

### 流式 API

```elisp
(magent-api-chat messages
  :stream t
  :callback (lambda (chunk)
              (insert chunk)))
```

### 流式传输工作原理

1. **服务器发送事件 (SSE)**: 两个提供商都使用 SSE 进行流式传输
2. **增量增量**: 在生成时接收部分响应
3. **事件解析**: 从 SSE 流解析 `data:` 行
4. **增量提取**: 从事件中提取文本增量

### 未来增强

完全流式支持需要：
1. 增量更新的缓冲区管理
2. 流式模式中的工具使用检测
3. 工具参数的部分 JSON 解析
4. 生成期间的 UI 更新

## 速率限制

### 提供商限制

**Anthropic：**
- 速率限制因计划等级而异
- 包括请求/分钟和令牌/分钟限制
- 返回带 retry-after 请求头的 429

**OpenAI：**
- 速率限制因模型和计划而异
- 以请求/分钟和令牌/分钟衡量
- 返回带 retry-after 请求头的 429

### 处理速率限制

目前，Magent 不实现自动重试逻辑。速率限制错误传播给用户。

**未来增强：**
```elisp
;; 建议的重试逻辑
(when (eq status 429)
  (let ((retry-after (cdr (assq 'retry-after headers))))
    (sleep-for (string-to-number retry-after))
    (magent-api-chat ...)))  ; 重试请求
```

## 成本优化

### 令牌用量

通过日志监控令牌用量：

```elisp
(magent-session-get-context-size session)
;; => 大致令牌计数
```

### 模型选择

选择合适的模型：
- **简单任务**: 使用更快/更便宜的模型 (Haiku, GPT-3.5)
- **复杂任务**: 使用更强大的模型 (Opus, GPT-4)
- **代码生成**: 平衡模型效果很好 (Sonnet, GPT-4-turbo)

### 代理特定模型

按代理覆盖模型：

```elisp
;; 在自定义代理文件中
---
model: claude-haiku-4-20250514
---
```

或以编程方式：

```elisp
(setf (magent-agent-info-model agent-info) "claude-haiku-4-20250514")
```

## 测试

### 模拟响应

用于在不调用 API 的情况下测试：

```elisp
;; 模拟 API 函数
(cl-defun magent-api-chat (messages &rest _)
  (funcall callback '((content . "Mock response"))))
```

### 本地模型

用本地模型进行测试：

```elisp
(setq magent-provider 'openai-compatible)
(setq magent-base-url "http://localhost:1234/v1")
(setq magent-model "local-model")
```

### 调试模式

启用详细日志：

```elisp
(setq magent-enable-logging t)
(setq url-debug t)  ; Emacs url 库调试
```

## 故障排查

### 找不到 API 密钥

```
Error: API key not set
```

**解决方案：**
```elisp
(setq magent-api-key "your-key")
;; 或
(setenv "ANTHROPIC_API_KEY" "your-key")
```

### 连接超时

```
Error: Connection timeout
```

**解决方案：**
```elisp
(setq magent-api--request-timeout 180)  ; 增加超时
```

### 无效模型

```
Error: Model not found
```

**解决方案：** 验证模型名称与提供商的模型匹配：
```elisp
;; 检查当前模型
magent-model

;; 更新为有效模型
(setq magent-model "claude-sonnet-4-20250514")
```

### SSL 证书问题

```
Error: Certificate verification failed
```

**解决方案：**
```elisp
;; 临时解决方案 (不建议用于生产)
(setq tls-checktrust nil)
```

### 超过速率限制

```
Error: Rate limit exceeded (429)
```

**解决方案：** 等待并重试，或升级 API 计划等级。

## 高级用法

### 自定义请求头

添加自定义请求头到请求：

```elisp
;; 修改 magent-api--get-headers
(defun magent-api--get-headers ()
  `(,@(default-headers)
    ("x-custom-header" . "value")))
```

### 请求拦截器

在发送前修改请求：

```elisp
;; 挂钩到 url-request-data
(advice-add 'magent-api-chat :before
  (lambda (&rest _)
    (message "Sending request...")))
```

### 响应转换器

接收后转换响应：

```elisp
(advice-add 'magent-api--parse-response :filter-return
  (lambda (response)
    ;; 添加自定义处理
    response))
```

## 提供商比较

| 特性 | Anthropic | OpenAI | OpenAI 兼容 |
|------|-----------|--------|------------|
| 工具调用 | 原生 | 原生 | 因提供商而异 |
| 流式传输 | 是 (SSE) | 是 (SSE) | 因提供商而异 |
| 上下文窗口 | 最多 200K 令牌 | 最多 128K 令牌 | 因提供商而异 |
| 响应格式 | 结构化块 | 简单内容 | 通常为 OpenAI 格式 |
| 图像支持 | 是 | 是 | 因提供商而异 |
| 函数调用 | tool_use 块 | tool_calls 数组 | 通常为 tool_calls |

## 最佳实践

1. **使用环境变量**: 将 API 密钥从代码中移出
2. **启用日志**: 在开发期间监控请求
3. **处理错误**: 在 API 调用中包装错误处理器
4. **选择合适的模型**: 平衡成本和能力
5. **监控令牌用量**: 通过日志跟踪成本
6. **先在本地测试**: 用本地模型进行开发
7. **实现重试**: 优雅地处理暂时性故障
8. **尽可能缓存**: 减少冗余的 API 调用
