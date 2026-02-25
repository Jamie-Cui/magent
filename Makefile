EMACS = emacs
GPTEL_DIR ?= $(HOME)/proj/gptel
LOADPATH = -L lisp -L $(GPTEL_DIR)

SRCS = lisp/magent.el \
       lisp/magent-config.el \
       lisp/magent-session.el \
       lisp/magent-tools.el \
       lisp/magent-agent.el \
       lisp/magent-agent-info.el \
       lisp/magent-agent-registry.el \
       lisp/magent-agent-types.el \
       lisp/magent-agent-file.el \
       lisp/magent-permission.el \
       lisp/magent-ui.el

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
