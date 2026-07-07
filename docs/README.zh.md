---
title: Magent 文档总览
lang: zh
alt_url: /
permalink: /zh/
---

# Magent 文档

欢迎阅读 Magent 文档。稳定的用户入口仍然放在仓库根目录的 `README.org`，本目录主要保存开发者指南、架构说明、工作流说明和故障排查材料。

## 本地预览

在仓库根目录运行：

```bash
scripts/serve-docs-local.sh
```

脚本默认在 `http://127.0.0.1:8099/` 托管站点。如果端口被占用，会自动选择后续可用端口并打印 URL。也可以指定监听地址或优先端口：

```bash
HOST=0.0.0.0 PORT=4000 scripts/serve-docs-local.sh
```

## 文档索引

### 快速开始

- **[ONBOARDING.zh.md](ONBOARDING.zh.html)**：新贡献者导览，包括架构分层、代码阅读路线和复杂点。
- **[TROUBLESHOOTING.zh.md](TROUBLESHOOTING.zh.html)**：常见问题、CI 问题和 live Emacs 调试流程。

### 架构

- **[ARCHITECTURE.zh.md](ARCHITECTURE.zh.html)**：产品定位、系统边界、模块层次、请求流和扩展模型。
- **[AGENT_WORKFLOW.zh.md](AGENT_WORKFLOW.zh.html)**：`thread -> turn -> item` 状态机、循环流程、`snapshot + journal` 持久化、UI 投影和 Codex 差异。
- **[AGENT_JOBS.zh.md](AGENT_JOBS.zh.html)**：durable child-agent job 生命周期、工具面、持久化、UI 和边界。
- **[UI_BACKENDS.zh.md](UI_BACKENDS.zh.html)**：UI backend 边界、agent-shell + ACP 流程、runtime API 和 legacy UI 隔离。

### 贡献

- **[CONTRIBUTING.zh.md](CONTRIBUTING.zh.html)**：贡献流程、代码风格、测试和 PR 要求。
- **[TROUBLESHOOTING.zh.md#github-actions-失败](TROUBLESHOOTING.zh.html#github-actions-失败)**：CI、live smoke 和 melpazoid 常见失败。

### 仓库根目录文档

- **[../README.org](https://github.com/jamie-cui/magent/blob/master/README.org)**：主 README，包含特性、安装和使用说明。
- **[../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md)**：面向 agentic coding 工具的开发指南，包含构建命令、架构要点和仓库约定。

## 推荐阅读路径

### 新贡献者

1. 先读 [ONBOARDING.zh.md](ONBOARDING.zh.html)。
2. 再读 [CONTRIBUTING.zh.md](CONTRIBUTING.zh.html) 了解开发流程。
3. 读 [../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md) 获取当前架构约束和 agent 开发规则。
4. 改 child-agent 生命周期前必须读 [AGENT_JOBS.zh.md](AGENT_JOBS.zh.html)。

### 用户

1. [../README.org](https://github.com/jamie-cui/magent/blob/master/README.org)：安装和配置。
2. [TROUBLESHOOTING.zh.md](TROUBLESHOOTING.zh.html)：常见问题。
3. 运行 `M-x magent-doctor` 做自诊断。
4. 使用 `C-c m ?` 打开 transient 菜单。

### 开发者

1. [CONTRIBUTING.zh.md](CONTRIBUTING.zh.html)：代码风格和 PR 流程。
2. [../AGENTS.md](https://github.com/jamie-cui/magent/blob/master/AGENTS.md)：构建、测试、架构和开发指令。
3. [ARCHITECTURE.zh.md](ARCHITECTURE.zh.html)：当前系统边界。
4. [ONBOARDING.zh.md](ONBOARDING.zh.html)：代码导览和复杂点。
5. [AGENT_JOBS.zh.md](AGENT_JOBS.zh.html)：child-agent job 架构。
6. [UI_BACKENDS.zh.md](UI_BACKENDS.zh.html)：UI backend 架构。

### CI 与打包

1. [../README.org](https://github.com/jamie-cui/magent/blob/master/README.org)：公开 workflow badge 和开发命令。
2. [CONTRIBUTING.zh.md#持续集成](CONTRIBUTING.zh.html#持续集成)：本地和 CI 验证顺序。
3. [TROUBLESHOOTING.zh.md#github-actions-失败](TROUBLESHOOTING.zh.html#github-actions-失败)：GitHub Actions 失败特征。
4. [../AGENTS.md#testing](https://github.com/jamie-cui/magent/blob/master/AGENTS.md#testing)：面向 agent 的测试、coverage、live smoke 和 melpazoid 说明。

## 文档维护标准

新增或修改文档时：

- 用户稳定入口优先更新根目录 `README.org`。
- 开发者文档放在 `docs/`。
- `docs/` 下优先使用 Markdown，除非有明确理由使用 Org。
- 新增英文文档时同步新增中文镜像页，并在导航和本索引中登记。
- 中断开发前更新稳定文档或活动任务笔记，让其他机器能从 git 恢复上下文。
