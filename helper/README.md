# Magent Happy Helper

Standalone TypeScript helper that bridges `magent-happy.el` and Happy sessions.

## Install

```bash
cd helper
npm install
npm run build
```

## Configure Emacs

```elisp
(setq magent-happy-helper-command
      '("node" "/path/to/magent/helper/dist/index.mjs"))
```

Optional environment:

- `HAPPY_SERVER_URL`
- `HAPPY_HOME_DIR`
- `MAGENT_HAPPY_PROJECT_PATH`
- `MAGENT_HAPPY_DEBUG=1`

## Usage

1. Authenticate with Happy so `~/.happy/agent.key` exists.
2. Build the helper.
3. In Emacs:

```elisp
(magent-happy-start-session)
```

or attach:

```elisp
(magent-happy-attach-session "session-prefix")
```

The helper speaks JSONL over stdio and is intended to be launched only by `magent-happy.el`.
