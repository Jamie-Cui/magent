# 会话管理

本文档描述了 Magent 如何管理对话状态、消息历史和会话持久化。

## 概述

Magent 中的会话（Session）维护与 AI 智能体的对话的上下文和历史。每个会话：

- 存储对话消息（用户、助手、工具结果）
- 跟踪分配的智能体
- 维护唯一标识符
- 可以持久化到磁盘并继续使用
- 自动管理历史大小

## 会话结构

### 数据结构

会话使用 `cl-defstruct` 定义：

```elisp
(cl-defstruct magent-session
  messages        ; 消息对象列表（最新的在前）
  max-history     ; 要保留的最大消息数
  id              ; 唯一会话标识符
  agent)          ; 当前分配的智能体信息
```

### 消息格式

消息遵循一致的内部格式：

```elisp
;; 用户消息
((role . user)
 (content . "Please read main.el"))

;; 包含文本的助手消息
((role . assistant)
 (content . "I'll read that file for you."))

;; 包含工具使用的助手消息
((role . assistant)
 (content . (((type . "text")
              (text . "I'll read that file."))
             ((type . "tool_use")
              (id . "call_123")
              (name . "read_file")
              (input . ((path . "main.el")))))))

;; 工具结果
((role . tool)
 (content . ((type . "tool_result")
             (tool_use_id . "call_123")
             (content . ";;; main.el contents..."))))
```

## 会话生命周期

### 1. 创建

会话在首次需要时懒加载创建：

```elisp
(defun magent-session-get ()
  "Get the current session, creating one if needed."
  (unless magent--current-session
    (setq magent--current-session (magent-session-create)))
  magent--current-session)
```

**新会话的属性：**
- 空消息列表
- 默认 max-history: `magent-max-history` (100)
- 首次保存前没有 ID
- 没有智能体分配（将使用默认值）

### 2. 智能体分配

首次处理提示时分配智能体：

```elisp
;; 自动分配到默认智能体
(magent-agent-process "prompt")  ; 使用默认智能体

;; 显式智能体选择
(magent-select-agent)  ; 交互式选择
```

**智能体持久性：**
- 智能体在会话生命周期内保持分配
- 可以使用 `magent-select-agent` 更改
- 保存以供继续使用时恢复

### 3. 消息累积

对话期间添加消息：

```elisp
;; 用户消息
(magent-session-add-message session 'user "Hello")

;; 助手消息
(magent-session-add-message session 'assistant "Hi there!")

;; 工具结果
(magent-session-add-tool-result session "call_123" "File contents...")
```

**消息顺序：**
- 内部存储：最新在前（prepend）
- API 使用：最旧在前（反转）
- 自动修剪到 max-history

### 4. 持久化

会话可以保存到磁盘：

```elisp
;; 保存当前会话
(magent-session-save session)
;; → ~/.emacs.d/magent-sessions/session-20250221-143015.json

;; 加载保存的会话
(magent-session-load "/path/to/session.json")
```

### 5. 重置/清除

会话可以被清除：

```elisp
;; 交互式
M-x magent-clear-session

;; 编程方式
(magent-session-reset)
```

## 会话操作

### 创建会话

**自动创建（推荐）：**
```elisp
(let ((session (magent-session-get)))
  ;; 使用会话
  )
```

**手动创建：**
```elisp
(let ((session (magent-session-create
                :max-history 50
                :agent (magent-agent-registry-get "explore"))))
  ;; 使用会话
  )
```

### 添加消息

**用户消息：**
```elisp
(magent-session-add-message session 'user "Find all .el files")
```

**助手消息：**
```elisp
(magent-session-add-message session 'assistant
  "I'll search for .el files using glob.")
```

**工具结果：**
```elisp
(magent-session-add-tool-result session "call_123"
  "file1.el\nfile2.el\nfile3.el")
```

### 检索消息

**所有消息（按时间顺序）：**
```elisp
(magent-session-get-messages session)
;; 返回从最旧到最新的消息
```

**消息计数：**
```elisp
(length (magent-session-messages session))
```

**最新消息：**
```elisp
(car (magent-session-messages session))
```

### 智能体管理

**获取分配的智能体：**
```elisp
(magent-session-get-agent session)
;; 返回 magent-agent-info 或 nil
```

**设置智能体：**
```elisp
(magent-session-set-agent session
  (magent-agent-registry-get "plan"))
```

**显示当前智能体：**
```elisp
M-x magent-show-current-agent
```

### 会话信息

**获取/生成会话 ID：**
```elisp
(magent-session-get-id session)
;; 返回现有 ID 或生成新 ID
;; 格式: "session-YYYYMMDD-HHMMSS"
```

**估计上下文大小：**
```elisp
(magent-session-get-context-size session)
;; 返回近似 token 计数（~4 字符/token）
```

**总结会话：**
```elisp
(magent-session-summarize session)
;; 返回最近消息的文本摘要
```

## 历史管理

### 自动修剪

会话自动修剪到 `max-history` 消息：

```elisp
(defun magent-session-add-message (session role content)
  "Add a message to SESSION."
  (let ((messages (magent-session-messages session)))
    (push (list (cons 'role role)
                (cons 'content content))
          messages)
    ;; 修剪到最大历史
    (when (> (length messages) (magent-session-max-history session))
      (setf messages (butlast messages
                      (- (length messages)
                         (magent-session-max-history session)))))
    (setf (magent-session-messages session) messages))
  session)
```

**行为：**
- 首先删除最旧的消息
- 每次添加消息时进行修剪
- 没有孤立工具使用/结果（限制）

### 配置历史大小

**全局默认值：**
```elisp
(setq magent-max-history 200)  ; 默认: 100
```

**按会话配置：**
```elisp
(let ((session (magent-session-create :max-history 50)))
  ;; 此会话限制为 50 条消息
  )
```

**禁用修剪：**
```elisp
(setf (magent-session-max-history session) most-positive-fixnum)
```

### 上下文大小管理

**监视上下文大小：**
```elisp
(let ((size (magent-session-get-context-size session)))
  (message "Approximate tokens: %d" size))
```

**何时进行压缩：**
- 上下文大小接近模型限制（例如 Sonnet 的 8K）
- 许多工具调用累积
- 会话感觉"缓慢"（大上下文）

**手动压缩**（未来功能）：
```elisp
;; 使用压缩智能体进行总结
(magent-agent-process-with-agent "Summarize this conversation"
                                 "compaction")
```

## 会话持久化

### 保存位置

**默认目录：**
```elisp
~/.emacs.d/magent-sessions/
```

**文件名格式：**
```
session-YYYYMMDD-HHMMSS.json
```

### 保存内容

**JSON 结构：**
```json
{
  "id": "session-20250221-143015",
  "messages": [
    {
      "role": "user",
      "content": "Hello"
    },
    {
      "role": "assistant",
      "content": "Hi there!"
    }
  ],
  "timestamp": "2025-02-21T14:30:15-0800"
}
```

**注意：** 智能体分配目前不被持久化（限制）。

### 保存会话

**手动保存：**
```elisp
(magent-session-save session)
;; 返回保存的文件路径

;; 保存到特定位置
(magent-session-save session "/custom/path/session.json")
```

**自动保存**（未实现）：
未来增强功能以在每次交互后自动保存。

### 加载会话

**从文件加载：**
```elisp
(let ((session (magent-session-load "/path/to/session.json")))
  (setq magent--current-session session))
```

**列出保存的会话：**
```elisp
(magent-session-list-saved)
;; 返回会话目录中 .json 文件的列表
```

**交互式加载**（未实现）：
用于会话浏览器 UI 的未来增强功能。

## 多会话管理

### 当前限制

Magent 目前使用单个全局会话：

```elisp
(defvar magent--current-session nil
  "The current active session.")
```

**影响：**
- 一次只有一个对话处于活跃状态
- 清除会话影响所有缓冲区
- 没有按缓冲区或按项目的会话

### 未来增强

潜在的多会话支持：

```elisp
;; 按缓冲区的会话
(defvar-local magent-buffer-session nil)

;; 按项目的会话
(defvar magent-project-sessions (make-hash-table :test 'equal))

;; 会话管理器 UI
(magent-session-manager)  ; 浏览/切换会话
```

## 会话显示

### 显示会话摘要

**交互式：**
```elisp
M-x magent-show-session
```

**显示格式：**
```
Session Summary:

[USER] Find all .el files
[ASSISTANT] I'll search for .el files using glob.
[TOOL] tool_use: glob
[TOOL] tool_result: file1.el\nfile2.el\nfile3.el
[ASSISTANT] I found 3 Emacs Lisp files.
```

### 会话统计

**按角色的消息计数：**
```elisp
(let ((messages (magent-session-get-messages session)))
  (cl-loop for msg in messages
           for role = (cdr (assq 'role msg))
           count (eq role 'user) into users
           count (eq role 'assistant) into assistants
           count (eq role 'tool) into tools
           finally return (list :user users
                               :assistant assistants
                               :tool tools)))
```

**工具使用统计：**
```elisp
;; 计数工具调用
(cl-loop for msg in (magent-session-get-messages session)
         when (eq (cdr (assq 'role msg)) 'tool)
         count t)
```

## 最佳实践

### 会话管理

1. **每个会话一个任务**：为不相关的任务启动新会话
2. **完成时清除**：完成工作后重置会话
3. **监视大小**：检查长对话的上下文大小
4. **保存重要会话**：持久化您可能稍后需要的会话
5. **适当的智能体**：在开始之前选择正确的智能体

### 消息管理

1. **简洁的提示**：保持用户消息重点突出
2. **上下文意识**：记住会话维护历史
3. **工具结果**：不要手动添加；由智能体循环处理
4. **清理旧会话**：定期删除保存的会话

### 历史大小

1. **平衡上下文与成本**：更大的历史 = 更多 token
2. **与任务相适应的大小**：简单任务需要较少的历史
3. **监视修剪**：知道何时旧消息会丢失
4. **必要时压缩**：对非常长的会话使用总结

### 智能体一致性

1. **每个会话同一智能体**：不要在对话中间切换
2. **与任务相适应的智能体**：从一开始使用正确的智能体
3. **谨慎重新分配**：只有在真正需要时才切换智能体

## 故障排除

### 会话状态问题

**会话似乎丢失：**
```elisp
;; 检查会话是否存在
magent--current-session

;; 验证消息
(magent-session-messages (magent-session-get))
```

**未分配智能体：**
```elisp
;; 检查智能体
(magent-session-get-agent (magent-session-get))

;; 手动分配
(magent-session-set-agent (magent-session-get)
  (magent-agent-registry-get "build"))
```

### 持久化问题

**保存失败：**
```elisp
;; 检查目录是否存在且可写
(file-writable-p (expand-file-name "magent-sessions" user-emacs-directory))

;; 创建目录
(make-directory (expand-file-name "magent-sessions" user-emacs-directory) t)
```

**加载失败：**
```elisp
;; 验证文件是否存在
(file-exists-p filepath)

;; 检查 JSON 有效性
;; 使用外部工具: jq . < session.json
```

### 上下文大小问题

**响应变慢：**
- 历史中有太多消息
- 工具结果非常长
- 上下文中的大文件内容

**解决方案：**
```elisp
;; 减少最大历史
(setq magent-max-history 50)

;; 清除并重启会话
M-x magent-clear-session

;; 使用总结（未来）
(magent-compact-session)
```

### 消息顺序混淆

**记住：**
- 内部存储：最新在前（性能）
- API 调用：最旧在前（正确的对话顺序）
- `magent-session-get-messages` 返回最旧在前

## 高级用法

### 自定义会话实现

**按项目的会话：**
```elisp
(defvar-local my-project-session nil)

(defun my-get-project-session ()
  "Get or create session for current project."
  (or my-project-session
      (setq my-project-session
            (magent-session-create
             :id (concat "project-" (projectile-project-name))))))
```

**预加载上下文的会话：**
```elisp
(defun my-create-contextual-session (files)
  "Create session with FILES pre-loaded as context."
  (let ((session (magent-session-create)))
    (dolist (file files)
      (magent-session-add-message session 'user
        (format "File %s:\n%s" file (magent-tools--read-file file))))
    session))
```

### 会话分析

**提取对话文本：**
```elisp
(defun my-session-text (session)
  "Extract plain text from SESSION messages."
  (mapconcat
   (lambda (msg)
     (let ((role (cdr (assq 'role msg)))
           (content (cdr (assq 'content msg))))
       (format "[%s] %s" role content)))
   (magent-session-get-messages session)
   "\n\n"))
```

**查找工具使用模式：**
```elisp
(defun my-session-tools-used (session)
  "List all tools used in SESSION."
  (cl-loop for msg in (magent-session-get-messages session)
           when (eq (cdr (assq 'role msg)) 'assistant)
           append (cl-loop for block in (cdr (assq 'content msg))
                          when (equal (cdr (assq 'type block)) "tool_use")
                          collect (cdr (assq 'name block)))))
```

## API 参考

### 函数

| 函数 | 目的 |
|------|------|
| `magent-session-create` | 创建新会话结构 |
| `magent-session-get` | 获取当前会话（如果需要则创建） |
| `magent-session-reset` | 清除当前会话 |
| `magent-session-add-message` | 添加用户/助手消息 |
| `magent-session-add-tool-result` | 添加工具执行结果 |
| `magent-session-get-messages` | 获取所有消息（按时间顺序） |
| `magent-session-get-agent` | 获取分配的智能体 |
| `magent-session-set-agent` | 设置分配的智能体 |
| `magent-session-get-id` | 获取/生成会话 ID |
| `magent-session-get-context-size` | 估计 token 计数 |
| `magent-session-summarize` | 生成文本摘要 |
| `magent-session-save` | 将会话持久化到磁盘 |
| `magent-session-load` | 从文件加载会话 |
| `magent-session-list-saved` | 列出保存的会话文件 |

### 交互式命令

| 命令 | 快捷键 | 描述 |
|------|--------|------|
| `magent-clear-session` | `C-c o c` | 清除当前会话 |
| `magent-show-session` | `C-c o s` | 显示会话摘要 |
| `magent-show-current-agent` | `C-c o i` | 显示分配的智能体 |
| `magent-select-agent` | `C-c o A` | 更改会话智能体 |
