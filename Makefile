EMACS = emacs
GPTEL_DIR ?= $(HOME)/proj/gptel
MARKDOWN_MODE_DIR ?= $(shell find $(HOME)/.emacs.d/elpa -maxdepth 1 -name 'markdown-mode-*' -type d 2>/dev/null | head -1)
LOADPATH = -L . -L $(GPTEL_DIR) $(if $(MARKDOWN_MODE_DIR),-L $(MARKDOWN_MODE_DIR))

SRCS = magent.el \
       magent-config.el \
       magent-session.el \
       magent-tools.el \
       magent-agent.el \
       magent-agent-registry.el \
       magent-agent-file.el \
       magent-permission.el \
       magent-ui.el \
       magent-fsm.el \
       magent-skill-emacs.el \
       magent-skills.el

COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean help

all: compile

help:
	@echo "Magent Emacs Lisp Implementation"
	@echo ""
	@echo "Targets:"
	@echo "  compile       - Byte compile all Elisp files"
	@echo "  clean         - Remove compiled files"
	@echo "  help          - Show this help message"

compile: $(COMPILED)

%.elc: %.el
	@echo "Compiling $<..."
	@$(EMACS) -Q --batch $(LOADPATH) -f batch-byte-compile $<

clean:
	@echo "Cleaning compiled files..."
	@rm -f $(COMPILED)
