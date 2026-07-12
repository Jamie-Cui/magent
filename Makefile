EMACS ?= emacs
EMACSCLIENT ?= emacsclient
EMACS_BATCH = $(EMACS) -Q --batch
COVERAGE_DIR ?= coverage
COVERAGE_MIN ?= 65
SOURCE_MANIFEST ?= source-files.txt

# Auto-detect dependency paths
GPTEL_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'gptel-[0-9]*' -type d 2>/dev/null | head -1)
TRANSIENT_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'transient-*' -type d 2>/dev/null | head -1)
ACP_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'acp-*' -type d 2>/dev/null | head -1)
SHELL_MAKER_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'shell-maker-*' -type d 2>/dev/null | head -1)
AGENT_SHELL_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'agent-shell-*' -type d 2>/dev/null | head -1)
COND_LET_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'cond-let-*' -type d 2>/dev/null | head -1)
COMPAT_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'compat-*' -type d 2>/dev/null | head -1)
EVIL_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'evil-*' -type d 2>/dev/null | head -1)
YAML_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'yaml-[0-9]*' -type d 2>/dev/null | head -1)
LLAMA_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'llama-*' -type d 2>/dev/null | head -1)
WITH_EDITOR_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'with-editor-*' -type d 2>/dev/null | head -1)

LOADPATH = -L lisp \
	$(if $(GPTEL_DIR),-L $(GPTEL_DIR)) \
	$(if $(TRANSIENT_DIR),-L $(TRANSIENT_DIR)) \
	$(if $(ACP_DIR),-L $(ACP_DIR)) \
	$(if $(SHELL_MAKER_DIR),-L $(SHELL_MAKER_DIR)) \
	$(if $(AGENT_SHELL_DIR),-L $(AGENT_SHELL_DIR)) \
	$(if $(COND_LET_DIR),-L $(COND_LET_DIR)) \
	$(if $(COMPAT_DIR),-L $(COMPAT_DIR)) \
	$(if $(EVIL_DIR),-L $(EVIL_DIR)) \
	$(if $(YAML_DIR),-L $(YAML_DIR)) \
	$(if $(LLAMA_DIR),-L $(LLAMA_DIR)) \
	$(if $(WITH_EDITOR_DIR),-L $(WITH_EDITOR_DIR))

# Compilation flags
BYTE_COMPILE_FLAGS = --eval "(setq byte-compile-error-on-warn nil)" \
	--eval "(setq byte-compile-warnings '(not cl-functions))"

SRCS = $(shell sed -e '/^[[:space:]]*#/d' -e '/^[[:space:]]*$$/d' $(SOURCE_MANIFEST))

COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean test test-unit test-live test-live-smoke coverage help

all: compile

help:
	@echo "Magent Emacs Lisp Implementation"
	@echo ""
	@echo "Targets:"
	@echo "  compile       - Byte compile all Elisp files"
	@echo "  test          - Run unit tests and deterministic live smoke tests"
	@echo "  test-unit     - Run batch unit tests"
	@echo "  test-live     - Run real live gptel tests; consumes tokens and requires Emacs server"
	@echo "  test-live-smoke - Run live Emacs smoke tests with stubbed gptel transport"
	@echo "  coverage      - Run ERT under built-in testcover"
	@echo "  clean         - Remove compiled files"
	@echo "  help          - Show this help message"

compile: $(COMPILED)
	@echo "Compilation complete: $(words $(COMPILED)) files"

lisp/%.elc: lisp/%.el
	@echo "Compiling $<..."
	@out=$$($(EMACS_BATCH) $(LOADPATH) $(BYTE_COMPILE_FLAGS) -f batch-byte-compile $< 2>&1); \
	status=$$?; \
	printf "%s\n" "$$out" | grep -v "^Compiling" | grep -v "^Wrote" || true; \
	exit $$status

test: test-unit test-live-smoke

test-unit:
	@echo "Running unit tests..."
	@$(EMACS) -Q --batch $(LOADPATH) \
		-l ert \
		-l test/magent-test.el \
		-f ert-run-tests-batch-and-exit

test-live:
	@echo "Running real live Emacs/gptel tests..."
	@$(EMACSCLIENT) -e "(progn (load-file \"$(CURDIR)/test/magent-live-test.el\") (magent-live-test-run))"

test-live-smoke:
	@echo "Running live Emacs smoke tests..."
	@$(EMACSCLIENT) -e "(progn (load-file \"$(CURDIR)/test/magent-live-test.el\") (magent-live-test-run-smoke))"

coverage:
	@echo "Running coverage..."
	@$(EMACS) -Q --batch $(LOADPATH) \
		--eval "(setq magent-coverage-directory \"$(COVERAGE_DIR)\" magent-coverage-min $(COVERAGE_MIN))" \
		-l test/coverage.el

clean:
	@echo "Cleaning compiled files..."
	@rm -f $(COMPILED)
