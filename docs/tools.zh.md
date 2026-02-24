# 工具参考

本文档描述了Magent代理可用于与文件和系统交互的工具。

## 概览

工具是代理与文件系统交互和执行命令的主要机制。每个工具注册为 `gptel-tool` 结构，具有：

- **名称 (Name)**: 在API调用中使用的唯一标识符
- **描述 (Description)**: 工具的功能（显示给LLM）
- **参数 (Args)**: 定义参数的 plist 列表
- **函数 (Function)**: 执行工具的Emacs Lisp函数
- **确认 (Confirm)**: 是否需要用户确认（用于 write_file、bash）
- **权限 (Permissions)**: 通过 Magent 权限系统实现的每个代理访问控制

## 可用工具

### read_file

读取文件的内容。

**目的:** 在进行更改前检查现有代码、配置文件或文档。

**参数:**
- `path` (string, 必需): 文件的绝对路径或相对路径

**返回:** 文件内容作为字符串，或错误消息

**示例:**
```json
{
  "name": "read_file",
  "input": {
    "path": "/home/user/project/src/main.el"
  }
}
```

**实现细节:**
- 使用 `insert-file-contents` 进行高效读取
- 在工具级别不强制大小限制
- 如果文件不存在或无法读取，返回错误消息
- 二进制文件按原样返回（可能包含控制字符）

**常见用途:**
- 编辑前读取源代码
- 检查配置文件
- 查看文档
- 检查测试文件

**权限:** 通常允许所有代理使用

---

### write_file

将内容写入文件，必要时创建父目录。

**目的:** 创建新文件或完全替换现有文件内容。

**参数:**
- `path` (string, 必需): 文件的绝对路径或相对路径
- `content` (string, 必需): 要写入文件的内容

**返回:** 成功消息或错误

**示例:**
```json
{
  "name": "write_file",
  "input": {
    "path": "/home/user/project/src/new-file.el",
    "content": ";;; new-file.el --- Description\n\n(provide 'new-file)"
  }
}
```

**实现细节:**
- 使用 `make-directory` 创建父目录
- 使用 `write-region` 进行原子写入
- 完全覆盖现有文件（无合并）
- 根据系统默认值设置文件权限

**安全考虑:**
- 可以覆盖任何可访问的文件
- 不会自动创建备份
- 受权限系统保护（例如，拒绝 `*.env`）

**常见用途:**
- 创建新源文件
- 生成配置文件
- 编写文档
- 创建测试文件

**权限:** 通常被代理限制（例如，plan代理只能写入 `.magent/plan/*.md`）

---

### grep

使用正则表达式搜索目录下文件中的模式。

**目的:** 查找代码模式、函数定义、变量使用或整个代码库中的文本。

**参数:**
- `pattern` (string, 必需): 要搜索的正则表达式
- `path` (string, 必需): 要搜索的目录（或单个文件）
- `case_sensitive` (boolean, 可选): 搜索是否区分大小写（默认: false）

**返回:** 以 `file:line:content` 格式匹配的行，用换行符分隔

**示例:**
```json
{
  "name": "grep",
  "input": {
    "pattern": "defun magent-.*-init",
    "path": "/home/user/project/lisp",
    "case_sensitive": false
  }
}
```

**输出格式:**
```
/home/user/project/lisp/magent.el:42:(defun magent-mode-init ()
/home/user/project/lisp/magent-agent.el:15:(defun magent-agent-init ()
```

**实现细节:**
- 递归搜索目录中的所有文件
- 跳过常见的非源文件目录：`.git`, `.svn`, `node_modules`, `.hg`
- 使用Emacs正则表达式语法（非POSIX）
- 默认不区分大小写
- 返回所有匹配项（无限制）

**性能考虑:**
- 在大代码库上可能较慢
- 搜索整个目录树
- 无并行化
- 考虑先使用 `glob` 缩小搜索范围

**常见用途:**
- 查找函数定义
- 定位变量使用
- 搜索TODO注释
- 查找错误消息
- 查找配置值

**正则表达式示例:**
```elisp
"defun .*foo"              ; 查找包含foo的函数定义
"TODO:.*"                  ; 查找TODO注释
"^import .*Component"      ; 查找导入（行开始）
"http[s]?://.*"           ; 查找URL
"[A-Z_]{3,}"              ; 查找常量（3个或以上大写字母）
```

**权限:** 通常允许大多数代理使用（只读操作）

---

### glob

查找与glob模式匹配的文件。

**目的:** 按名称模式、扩展名或路径结构定位文件。

**参数:**
- `pattern` (string, 必需): Glob模式（支持 `*` 和 `**`）
- `path` (string, 必需): 要搜索的目录

**返回:** 换行符分隔的匹配文件路径列表

**示例:**
```json
{
  "name": "glob",
  "input": {
    "pattern": "**/*.el",
    "path": "/home/user/project"
  }
}
```

**输出格式:**
```
/home/user/project/lisp/magent.el
/home/user/project/lisp/magent-agent.el
/home/user/project/lisp/magent-api.el
```

**Glob模式:**
- `*`: 匹配单个路径组件中的任何字符
- `**`: 匹配任何字符，包括路径分隔符（递归）
- `?`: 匹配任何单个字符
- `[abc]`: 匹配括号内的任何字符
- `{a,b}`: 匹配任一模式

**模式示例:**
```
*.el                    ; 目录中的所有 .el 文件
**/*.el                 ; 递归地所有 .el 文件
src/**/*.js             ; src/ 下的所有 .js 文件
test-*.el               ; 以 test- 开头的文件
**/*-test.el            ; 以 -test.el 结尾的文件
{src,lib}/**/*.el       ; src/ 或 lib/ 中的 .el 文件
```

**实现细节:**
- 使用Emacs `file-expand-wildcards`
- 相对于指定的路径
- 返回绝对路径
- 按字母顺序排序
- 如果模式匹配，包括隐藏文件

**性能考虑:**
- 简单模式速度快
- 深度递归模式速度较慢
- 调用之间无缓存

**常见用途:**
- 查找特定类型的所有源文件
- 定位测试文件
- 查找配置文件
- 发现文档文件

**权限:** 通常允许大多数代理使用（只读操作）

---

### bash

执行shell命令。

**目的:** 运行构建工具、版本控制、包管理器或任何命令行操作。

**参数:**
- `command` (string, 必需): 要执行的shell命令
- `timeout` (integer, 可选): 超时（秒）（默认: 30）

**返回:** 合并的stdout和stderr输出，或错误消息

**示例:**
```json
{
  "name": "bash",
  "input": {
    "command": "git status --short",
    "timeout": 10
  }
}
```

**实现细节:**
- 使用 `shell-command-to-string` 执行
- 在用户默认shell中运行
- 通过 `with-timeout` 提供超时保护
- 合并stdout和stderr
- 如果输出为空，返回"Command completed with no output"

**安全考虑:**
- **高风险**: 可以执行任意命令
- 工具级别无沙箱或限制
- 受权限系统保护
- 许多代理拒绝bash或要求 `ask` 权限

**常见用途:**
- 运行测试: `npm test`, `pytest`
- 构建: `make`, `cargo build`
- 版本控制: `git status`, `git diff`
- 包管理: `npm install`, `pip install`
- 文件操作: `ls`, `find`, `cp`
- Linting: `eslint`, `rubocop`

**超时行为:**
- 默认超时: 30秒
- 每个调用可配置
- 超时时抛出错误
- 命令可能在后台继续运行（限制）

**工作目录:**
- 使用当前Emacs `default-directory`
- 通常是项目根目录

**命令示例:**
```bash
git log --oneline -10                    # 最近的提交
npm test -- --watch=false                # 运行一次测试
find . -name "*.pyc" -delete            # 清理Python缓存
grep -r "TODO" --include="*.el" .       # 搜索TODO
make clean && make                       # 重建项目
```

**权限:** 通常被限制（审查代理拒绝，构建代理要求 `ask`）

---

## 工具执行流程

1. **工具注册**: 在加载时将工具注册为 `gptel-tool` 结构
2. **权限过滤**: `magent-tools-get-gptel-tools` 按代理过滤工具
3. **gptel 请求**: 将过滤后的工具传递给 `gptel-request`
4. **LLM 响应**: LLM 在响应中生成工具使用
5. **gptel 执行**: gptel 调用工具的 `:function`，可选提示用户确认（`:confirm t`）
6. **结果处理**: gptel 将工具结果发送回 LLM 并继续循环
7. **回调**: Magent 的回调接收工具调用通知用于 UI 显示

## 工具定义

工具使用 `gptel-make-tool` 定义为 `gptel-tool` 结构：

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

具有副作用的工具使用 `:confirm t`：

```elisp
(gptel-make-tool
 :name "write_file"
 :description "Write content to a file."
 :args (list '(:name "path" :type string :description "File path")
             '(:name "content" :type string :description "Content to write"))
 :function #'magent-tools--write-file
 :confirm t
 :category "magent")
```

## 错误处理

所有工具使用一致的错误处理：

```elisp
(condition-case err
    ;; 工具实现
  (error
    (format "Error [operation]: %s" (error-message-string err))))
```

**错误消息格式:**
```
Error reading file: permission denied
Error writing file: directory does not exist
Error during grep: invalid regexp
Error during glob: directory not accessible
Error executing command: timeout exceeded
```

## 路径解析

### 相对路径

相对于 `default-directory` 解析（通常是项目根目录）：

```
"src/main.el"  → "/home/user/project/src/main.el"
```

### 绝对路径

按原样使用：

```
"/home/user/project/src/main.el"  → "/home/user/project/src/main.el"
```

### 主目录

支持Tilde展开：

```
"~/project/file.el"  → "/home/user/project/file.el"
```

## 工具性能

### read_file
- **速度**: 快（小文件 < 1ms）
- **扩展性**: 与文件大小成线性
- **限制**: 无内置限制
- **内存**: 将整个文件加载到内存中

### write_file
- **速度**: 快（小文件 < 10ms）
- **扩展性**: 与内容大小成线性
- **限制**: 无内置限制
- **内存**: 写入期间整个内容在内存中

### grep
- **速度**: 大代码库上较慢（秒级）
- **扩展性**: 与文件数 × 平均文件大小成线性
- **限制**: 无结果限制（可返回数千个匹配）
- **内存**: 一次处理一个文件

### glob
- **速度**: 简单模式快（< 100ms）
- **扩展性**: 取决于目录深度和文件数
- **限制**: 无限制
- **内存**: 所有路径存储在内存中

### bash
- **速度**: 取决于命令
- **扩展性**: 取决于命令
- **限制**: 默认30秒超时
- **内存**: 输出在内存中缓冲

## 工具最佳实践

### read_file
1. **编辑前检查**: 始终在修改前读取文件
2. **验证路径**: 确保文件存在后再读取
3. **处理大文件**: 注意内存使用
4. **二进制文件**: 可能包含不可打印字符

### write_file
1. **原子写入**: 文件原子地被替换
2. **无备份**: 前一个内容丢失
3. **路径验证**: 确保路径正确
4. **行结尾**: 保持平台约定

### grep
1. **缩小范围**: 搜索特定目录，不是整个文件系统
2. **优化模式**: 使用特定模式以减少匹配
3. **区分大小写**: 通常需要不区分大小写
4. **转义特殊字符**: 正则表达式元字符需要转义

### glob
1. **特定模式**: 窄模式速度更快
2. **避免根目录搜索**: 不要从 `/` 搜索
3. **使用扩展名**: `*.el` 优于 `*`
4. **与grep结合**: 使用glob查找文件，grep搜索内容

### bash
1. **适当超时**: 设置合理的超时
2. **检查退出码**: 解析输出以判断成功/失败
3. **避免交互命令**: 无法进行用户输入
4. **引用参数**: 使用正确的shell引引
5. **首选本地工具**: 使用read/write而不是cat/echo

## 工具组合

使用多个工具的有效模式：

### 查找和读取模式
```
1. glob("**/*.el", ".")          → 查找所有 .el 文件
2. read_file("file1.el")         → 读取特定文件
3. read_file("file2.el")         → 读取另一文件
```

### 搜索和修改模式
```
1. grep("defun foo", ".")        → 查找函数定义
2. read_file("src/foo.el")       → 读取文件
3. write_file("src/foo.el", ...) → 更新文件
```

### 构建和测试模式
```
1. write_file("src/new.el", ...) → 创建新代码
2. bash("make compile")          → 构建项目
3. bash("make test")             → 运行测试
```

### 探索和理解模式
```
1. glob("**/*", "src/")          → 列出所有源文件
2. grep("class.*Controller", ".")→ 查找控制器
3. read_file("main_controller.el")→ 读取实现
```

## 工具配置

### 启用/禁用工具

全局配置：

```elisp
(setq magent-enable-tools '(read write grep glob))  ; 无bash
```

### 每个代理的权限

在自定义代理文件中：

```yaml
tools:
  bash: false     # 为此代理禁用bash
  write: false    # 只读代理
  read: true
  grep: true
  glob: true
```

## 扩展工具

### 添加新工具

1. **在 `magent-tools.el` 中实现函数**:
```elisp
(defun magent-tools--mytool (param1 param2)
  "此工具的功能描述。"
  (condition-case err
      ;; 实现
    (error (format "Error in mytool: %s" (error-message-string err)))))
```

2. **注册为 gptel-tool 结构**:
```elisp
(defvar magent-tools--mytool-tool
  (gptel-make-tool
   :name "mytool"
   :description "为LLM的描述"
   :args (list '(:name "param1" :type string :description "第一个参数")
               '(:name "param2" :type string :description "第二个参数"))
   :function #'magent-tools--mytool
   :category "magent")
  "gptel-tool struct for mytool.")
```

3. **添加到全局工具列表**:
```elisp
;; 添加到 magent-tools--all-gptel-tools
(push magent-tools--mytool-tool magent-tools--all-gptel-tools)
```

4. **添加权限键映射**（用于过滤）:
```elisp
;; 添加到 magent-tools--name-to-permission-key
(push '("mytool" . mytool) magent-tools--name-to-permission-key)
```

5. **可选添加到分发器**（用于直接使用）:
```elisp
(pcase tool-name
  ("mytool"
   (magent-tools--mytool (cdr (assq 'param1 input))
                         (cdr (assq 'param2 input)))))
```

6. **默认启用**:
```elisp
(setq magent-enable-tools '(read write grep glob bash mytool))
```

### 工具开发指南

1. **错误处理**: 始终包装在 `condition-case` 中
2. **返回字符串**: 所有工具返回字符串结果
3. **描述性错误**: 在错误消息中包含上下文
4. **参数验证**: 检查必需参数
5. **文档**: 为LLM提供清晰的描述
6. **幂等性**: 相同输入 → 相同输出
7. **无副作用**: 避免全局状态改变（除了文件I/O）

## 故障排除

### 工具权限拒绝

```
Error: Permission denied: tool 'write_file' not allowed
```

**解决方案:** 检查代理权限：
```elisp
(magent-agent-info-permission agent)
```

### 工具未找到

```
Error: Unknown tool: mytool
```

**解决方案:**
1. 检查工具是否在 `magent-enable-tools` 中
2. 验证工具名称拼写
3. 确保工具定义存在

### 路径未找到

```
Error reading file: no such file or directory
```

**解决方案:**
1. 使用绝对路径
2. 验证文件存在：`(file-exists-p path)`
3. 检查工作目录：`default-directory`

### Grep/Glob无结果

**解决方案:**
1. 验证模式语法
2. 检查目录路径是否正确
3. 确保文件存在于该位置
4. 首先尝试更简单的模式

### Bash超时

```
Error executing command: timeout exceeded
```

**解决方案:**
1. 增加超时参数
2. 优化命令以加快速度
3. 以不同方式在后台运行长任务
4. 检查命令是否挂起

### Bash命令未找到

```
bash: command not found: mytool
```

**解决方案:**
1. 检查命令是否已安装
2. 验证PATH包含命令位置
3. 使用命令的绝对路径
4. 检查Emacs `exec-path` 变量
