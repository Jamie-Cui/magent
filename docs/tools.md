# Tools Reference

This document describes the tools available to Magent agents for interacting with files and the system.

## Overview

Tools are the primary mechanism by which agents interact with the file system and execute commands. Each tool has:

- **Name**: Unique identifier used in API calls
- **Description**: What the tool does (shown to LLM)
- **Input Schema**: JSON schema defining parameters
- **Implementation**: Emacs Lisp function that executes the tool
- **Permissions**: Per-agent access control

## Available Tools

### read_file

Read the contents of a file.

**Purpose:** Examine existing code, configuration files, or documentation before making changes.

**Parameters:**
- `path` (string, required): Absolute or relative path to the file

**Returns:** File contents as a string, or error message

**Example:**
```json
{
  "name": "read_file",
  "input": {
    "path": "/home/user/project/src/main.el"
  }
}
```

**Implementation Details:**
- Uses `insert-file-contents` for efficient reading
- No size limits enforced at tool level
- Returns error message if file doesn't exist or isn't readable
- Binary files returned as-is (may contain control characters)

**Common Use Cases:**
- Reading source code before editing
- Examining configuration files
- Reviewing documentation
- Checking test files

**Permissions:** Usually allowed for all agents

---

### write_file

Write content to a file, creating parent directories if needed.

**Purpose:** Create new files or completely replace existing file contents.

**Parameters:**
- `path` (string, required): Absolute or relative path to the file
- `content` (string, required): Content to write to the file

**Returns:** Success message or error

**Example:**
```json
{
  "name": "write_file",
  "input": {
    "path": "/home/user/project/src/new-file.el",
    "content": ";;; new-file.el --- Description\n\n(provide 'new-file)"
  }
}
```

**Implementation Details:**
- Creates parent directories with `make-directory`
- Uses `write-region` for atomic writes
- Overwrites existing files completely (no merging)
- Sets file permissions based on system defaults

**Security Considerations:**
- Can overwrite any accessible file
- No backup created automatically
- Protected by permission system (e.g., deny `*.env`)

**Common Use Cases:**
- Creating new source files
- Generating configuration files
- Writing documentation
- Creating test files

**Permissions:** Often restricted by agents (e.g., plan agent can only write to `.opencode/plan/*.md`)

---

### grep

Search for patterns in files under a directory using regular expressions.

**Purpose:** Find code patterns, function definitions, variable usage, or text across the codebase.

**Parameters:**
- `pattern` (string, required): Regular expression to search for
- `path` (string, required): Directory to search in (or single file)
- `case_sensitive` (boolean, optional): Whether search is case-sensitive (default: false)

**Returns:** Matching lines in `file:line:content` format, separated by newlines

**Example:**
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

**Output Format:**
```
/home/user/project/lisp/magent.el:42:(defun magent-mode-init ()
/home/user/project/lisp/magent-agent.el:15:(defun magent-agent-init ()
```

**Implementation Details:**
- Recursively searches all files in directory
- Skips common non-source directories: `.git`, `.svn`, `node_modules`, `.hg`
- Uses Emacs regex syntax (not POSIX)
- Case-insensitive by default
- Returns all matches (no limit)

**Performance Considerations:**
- Can be slow on large codebases
- Searches entire directory tree
- No parallelization
- Consider using `glob` first to narrow search scope

**Common Use Cases:**
- Finding function definitions
- Locating variable usage
- Searching for TODO comments
- Finding error messages
- Locating configuration values

**Regex Examples:**
```elisp
"defun .*foo"              ; Find function definitions containing foo
"TODO:.*"                  ; Find TODO comments
"^import .*Component"      ; Find imports (line start)
"http[s]?://.*"           ; Find URLs
"[A-Z_]{3,}"              ; Find constants (3+ uppercase letters)
```

**Permissions:** Usually allowed for most agents (read-only operation)

---

### glob

Find files matching a glob pattern.

**Purpose:** Locate files by name pattern, extension, or path structure.

**Parameters:**
- `pattern` (string, required): Glob pattern (supports `*` and `**`)
- `path` (string, required): Directory to search in

**Returns:** Newline-separated list of matching file paths

**Example:**
```json
{
  "name": "glob",
  "input": {
    "pattern": "**/*.el",
    "path": "/home/user/project"
  }
}
```

**Output Format:**
```
/home/user/project/lisp/magent.el
/home/user/project/lisp/magent-agent.el
/home/user/project/lisp/magent-api.el
```

**Glob Patterns:**
- `*`: Matches any characters in a single path component
- `**`: Matches any characters including path separators (recursive)
- `?`: Matches any single character
- `[abc]`: Matches any character in brackets
- `{a,b}`: Matches either pattern

**Pattern Examples:**
```
*.el                    ; All .el files in directory
**/*.el                 ; All .el files recursively
src/**/*.js             ; All .js files under src/
test-*.el               ; Files starting with test-
**/*-test.el            ; Files ending with -test.el
{src,lib}/**/*.el       ; .el files in src/ or lib/
```

**Implementation Details:**
- Uses Emacs `file-expand-wildcards`
- Relative to specified path
- Returns absolute paths
- Sorted alphabetically
- Includes hidden files if pattern matches

**Performance Considerations:**
- Fast for simple patterns
- Slower for deep recursive patterns
- No caching between calls

**Common Use Cases:**
- Finding all source files of a type
- Locating test files
- Finding configuration files
- Discovering documentation files

**Permissions:** Usually allowed for most agents (read-only operation)

---

### bash

Execute a shell command.

**Purpose:** Run build tools, version control, package managers, or any command-line operation.

**Parameters:**
- `command` (string, required): Shell command to execute
- `timeout` (integer, optional): Timeout in seconds (default: 30)

**Returns:** Combined stdout and stderr output, or error message

**Example:**
```json
{
  "name": "bash",
  "input": {
    "command": "git status --short",
    "timeout": 10
  }
}
```

**Implementation Details:**
- Uses `shell-command-to-string` for execution
- Runs in user's default shell
- Timeout protection via `with-timeout`
- Combines stdout and stderr
- Returns "Command completed with no output" if output is empty

**Security Considerations:**
- **HIGH RISK**: Can execute arbitrary commands
- No sandboxing or restrictions at tool level
- Protected by permission system
- Many agents deny bash or require `ask` permission

**Common Use Cases:**
- Running tests: `npm test`, `pytest`
- Building: `make`, `cargo build`
- Version control: `git status`, `git diff`
- Package management: `npm install`, `pip install`
- File operations: `ls`, `find`, `cp`
- Linting: `eslint`, `rubocop`

**Timeout Behavior:**
- Default timeout: 30 seconds
- Configurable per call
- Throws error on timeout
- Command may continue running in background (limitation)

**Working Directory:**
- Uses current Emacs `default-directory`
- Typically the project root

**Command Examples:**
```bash
git log --oneline -10                    # Recent commits
npm test -- --watch=false                # Run tests once
find . -name "*.pyc" -delete            # Clean Python cache
grep -r "TODO" --include="*.el" .       # Search TODOs
make clean && make                       # Rebuild project
```

**Permissions:** Often restricted (deny for review agents, ask for build agents)

---

## Tool Execution Flow

1. **Agent Request**: LLM generates tool use in response
2. **Permission Check**: Verify agent has permission for tool
3. **Parameter Validation**: Ensure required parameters present
4. **Tool Execution**: Call Emacs Lisp implementation
5. **Result Capture**: Catch errors, format output
6. **Session Update**: Add tool result to conversation history
7. **Agent Loop**: Continue with next iteration

## Tool Schemas

Tools are defined with JSON schemas for the LLM:

```elisp
((name . "read_file")
 (description . "Read the contents of a file. Use this to see the current state of a file before making changes.")
 (input_schema
   (type . "object")
   (properties . ((path . ((type . "string")
                           (description . "Absolute or relative path to the file")))))
   (required . (path))
   (additionalProperties . :json-false)))
```

## Error Handling

All tools use consistent error handling:

```elisp
(condition-case err
    ;; Tool implementation
  (error
    (format "Error [operation]: %s" (error-message-string err))))
```

**Error Message Format:**
```
Error reading file: permission denied
Error writing file: directory does not exist
Error during grep: invalid regexp
Error during glob: directory not accessible
Error executing command: timeout exceeded
```

## Path Resolution

### Relative Paths

Resolved relative to `default-directory` (usually project root):

```
"src/main.el"  → "/home/user/project/src/main.el"
```

### Absolute Paths

Used as-is:

```
"/home/user/project/src/main.el"  → "/home/user/project/src/main.el"
```

### Home Directory

Tilde expansion supported by Emacs:

```
"~/project/file.el"  → "/home/user/project/file.el"
```

## Tool Performance

### read_file
- **Speed**: Fast (< 1ms for small files)
- **Scaling**: Linear with file size
- **Limit**: No built-in limit
- **Memory**: Loads entire file into memory

### write_file
- **Speed**: Fast (< 10ms for small files)
- **Scaling**: Linear with content size
- **Limit**: No built-in limit
- **Memory**: Entire content in memory during write

### grep
- **Speed**: Slow for large codebases (seconds)
- **Scaling**: Linear with number of files × average file size
- **Limit**: No result limit (can return thousands of matches)
- **Memory**: Processes files one at a time

### glob
- **Speed**: Fast for simple patterns (< 100ms)
- **Scaling**: Depends on directory depth and file count
- **Limit**: No limit
- **Memory**: All paths stored in memory

### bash
- **Speed**: Depends on command
- **Scaling**: Depends on command
- **Limit**: 30 second default timeout
- **Memory**: Output buffered in memory

## Tool Best Practices

### read_file
1. **Check before edit**: Always read files before modifying
2. **Verify path**: Ensure file exists before reading
3. **Handle large files**: Be aware of memory usage
4. **Binary files**: May contain non-printable characters

### write_file
1. **Atomic writes**: File is replaced atomically
2. **No backup**: Previous content is lost
3. **Path validation**: Ensure path is correct
4. **Line endings**: Preserve platform conventions

### grep
1. **Narrow scope**: Search specific directories, not entire filesystem
2. **Optimize patterns**: Use specific patterns to reduce matches
3. **Case sensitivity**: Usually want case-insensitive
4. **Escape special chars**: Regex metacharacters need escaping

### glob
1. **Specific patterns**: Narrow patterns are faster
2. **Avoid root search**: Don't search from `/`
3. **Use extensions**: `*.el` better than `*`
4. **Combine with grep**: Use glob to find files, grep to search content

### bash
1. **Timeout appropriately**: Set reasonable timeouts
2. **Check exit codes**: Parse output for success/failure
3. **Avoid interactive commands**: No user input possible
4. **Quote arguments**: Use proper shell quoting
5. **Prefer native tools**: Use read/write instead of cat/echo

## Tool Combinations

Effective patterns using multiple tools:

### Find and Read Pattern
```
1. glob("**/*.el", ".")          → Find all .el files
2. read_file("file1.el")         → Read specific file
3. read_file("file2.el")         → Read another file
```

### Search and Modify Pattern
```
1. grep("defun foo", ".")        → Find function definition
2. read_file("src/foo.el")       → Read the file
3. write_file("src/foo.el", ...) → Update the file
```

### Build and Test Pattern
```
1. write_file("src/new.el", ...) → Create new code
2. bash("make compile")          → Build project
3. bash("make test")             → Run tests
```

### Explore and Understand Pattern
```
1. glob("**/*", "src/")          → List all source files
2. grep("class.*Controller", ".")→ Find controllers
3. read_file("main_controller.el")→ Read implementation
```

## Tool Configuration

### Enable/Disable Tools

Configure globally:

```elisp
(setq magent-enable-tools '(read write grep glob))  ; No bash
```

### Per-Agent Permissions

In custom agent file:

```yaml
tools:
  bash: false     # Disable bash for this agent
  write: false    # Read-only agent
  read: true
  grep: true
  glob: true
```

## Extending Tools

### Adding New Tools

1. **Implement function** in `magent-tools.el`:
```elisp
(defun magent-tools--mytool (param1 param2)
  "Description of what this tool does."
  (condition-case err
      ;; Implementation
    (error (format "Error in mytool: %s" (error-message-string err)))))
```

2. **Add to definitions**:
```elisp
(push '((name . "mytool")
        (description . "Description for LLM")
        (input_schema ...))
      tools)
```

3. **Add to dispatcher**:
```elisp
(pcase tool-name
  ("mytool"
   (magent-tools--mytool (cdr (assq 'param1 input))
                         (cdr (assq 'param2 input)))))
```

4. **Enable by default**:
```elisp
(setq magent-enable-tools '(read write grep glob bash mytool))
```

### Tool Development Guidelines

1. **Error handling**: Always wrap in `condition-case`
2. **Return strings**: All tools return string results
3. **Descriptive errors**: Include context in error messages
4. **Parameter validation**: Check for required parameters
5. **Documentation**: Clear description for LLM
6. **Idempotency**: Same inputs → same outputs
7. **No side effects**: Avoid global state changes (except file I/O)

## Troubleshooting

### Tool Permission Denied

```
Error: Permission denied: tool 'write_file' not allowed
```

**Solution:** Check agent permissions:
```elisp
(magent-agent-info-permission agent)
```

### Tool Not Found

```
Error: Unknown tool: mytool
```

**Solutions:**
1. Check tool is in `magent-enable-tools`
2. Verify tool name spelling
3. Ensure tool definition exists

### Path Not Found

```
Error reading file: no such file or directory
```

**Solutions:**
1. Use absolute paths
2. Verify file exists: `(file-exists-p path)`
3. Check working directory: `default-directory`

### Grep/Glob No Results

**Solutions:**
1. Verify pattern syntax
2. Check directory path is correct
3. Ensure files exist in that location
4. Try simpler pattern first

### Bash Timeout

```
Error executing command: timeout exceeded
```

**Solutions:**
1. Increase timeout parameter
2. Optimize command for speed
3. Run long tasks in background differently
4. Check if command is hanging

### Bash Command Not Found

```
bash: command not found: mytool
```

**Solutions:**
1. Check command is installed
2. Verify PATH includes command location
3. Use absolute path to command
4. Check Emacs `exec-path` variable
