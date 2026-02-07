EMACS = emacs
LOADPATH = -L lisp

SRCS = lisp/magent.el \
       lisp/magent-config.el \
       lisp/magent-api.el \
       lisp/magent-session.el \
       lisp/magent-tools.el \
       lisp/magent-agent.el \
       lisp/magent-ui.el

COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean test help

all: compile

help:
	@echo "Magent Emacs Lisp Implementation"
	@echo ""
	@echo "Targets:"
	@echo "  compile   - Byte compile all Elisp files"
	@echo "  clean     - Remove compiled files"
	@echo "  test      - Run tests"
	@echo "  help      - Show this help message"

compile: $(COMPILED)

lisp/%.elc: lisp/%.el
	@echo "Compiling $<..."
	$(EMACS) -Q --batch $(LOADPATH) -f batch-byte-compile $<

clean:
	@echo "Cleaning compiled files..."
	rm -f $(COMPILED)

test: compile
	@echo "Running tests..."
	$(EMACS) -Q --batch $(LOADPATH) -l lisp/magent-tests.el -f ert-run-tests-batch-and-exit
