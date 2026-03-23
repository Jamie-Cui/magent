EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch

# Auto-detect dependency paths
GPTEL_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'gptel-[0-9]*' -type d 2>/dev/null | head -1)
MAGIT_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'magit-*' -type d 2>/dev/null | head -1)
SPINNER_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'spinner-*' -type d 2>/dev/null | head -1)
TRANSIENT_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'transient-*' -type d 2>/dev/null | head -1)
COND_LET_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'cond-let-*' -type d 2>/dev/null | head -1)
EVIL_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'evil-*' -type d 2>/dev/null | head -1)
YAML_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'yaml-[0-9]*' -type d 2>/dev/null | head -1)

LOADPATH = -L . \
	$(if $(GPTEL_DIR),-L $(GPTEL_DIR)) \
	$(if $(MAGIT_DIR),-L $(MAGIT_DIR)) \
	$(if $(SPINNER_DIR),-L $(SPINNER_DIR)) \
	$(if $(TRANSIENT_DIR),-L $(TRANSIENT_DIR)) \
	$(if $(COND_LET_DIR),-L $(COND_LET_DIR)) \
	$(if $(EVIL_DIR),-L $(EVIL_DIR)) \
	$(if $(YAML_DIR),-L $(YAML_DIR))

# Compilation flags
BYTE_COMPILE_FLAGS = --eval "(setq byte-compile-error-on-warn nil)" \
	--eval "(setq byte-compile-warnings '(not cl-functions obsolete))"

SRCS = magent-config.el \
       magent-audit.el \
       magent.el \
       magent-events.el \
       magent-approval.el \
       magent-file-loader.el \
       magent-session.el \
       magent-tools.el \
       magent-agent.el \
       magent-agent-registry.el \
       magent-permission.el \
       magent-ui.el \
       magent-magit.el \
       magent-fsm.el \
       magent-fsm-shared.el \
       magent-skills.el \
       magent-capability.el \
       magent-capability-file.el

COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean test help

all: compile

help:
	@echo "Magent Emacs Lisp Implementation"
	@echo ""
	@echo "Targets:"
	@echo "  compile       - Byte compile all Elisp files"
	@echo "  test          - Run unit tests"
	@echo "  clean         - Remove compiled files"
	@echo "  help          - Show this help message"

compile: $(COMPILED)
	@echo "Compilation complete: $(words $(COMPILED)) files"

%.elc: %.el
	@echo "Compiling $<..."
	@$(EMACS_BATCH) $(LOADPATH) $(BYTE_COMPILE_FLAGS) -f batch-byte-compile $< 2>&1 | \
		grep -v "^Compiling" | grep -v "^Wrote" || true

test:
	@echo "Running unit tests..."
	@$(EMACS) -Q --batch $(LOADPATH) \
		-l ert \
		-l test/magent-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning compiled files..."
	@rm -f $(COMPILED)
