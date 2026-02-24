EMACS = emacs
GPTEL_DIR ?= $(HOME)/proj/gptel
LOADPATH = -L lisp -L test -L $(GPTEL_DIR)

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

TEST_SRCS = test/magent-test-helper.el \
            test/magent-agent-info-test.el \
            test/magent-permission-test.el \
            test/magent-session-test.el \
            test/magent-agent-registry-test.el

COMPILED = $(SRCS:.el=.elc)
TEST_COMPILED = $(TEST_SRCS:.el=.elc)

.PHONY: all compile compile-tests clean clean-tests test test-only help

all: compile

help:
	@echo "Magent Emacs Lisp Implementation"
	@echo ""
	@echo "Targets:"
	@echo "  compile       - Byte compile all Elisp files"
	@echo "  compile-tests - Byte compile test files"
	@echo "  clean         - Remove compiled files"
	@echo "  clean-tests   - Remove compiled test files"
	@echo "  test          - Compile and run tests"
	@echo "  test-only     - Run tests without compiling"
	@echo "  help          - Show this help message"

compile: $(COMPILED)

compile-tests: $(TEST_COMPILED)

lisp/%.elc: lisp/%.el
	@echo "Compiling $<..."
	@$(EMACS) -Q --batch $(LOADPATH) -f batch-byte-compile $<

test/%.elc: test/%.el
	@echo "Compiling $<..."
	@$(EMACS) -Q --batch $(LOADPATH) -f batch-byte-compile $<

clean:
	@echo "Cleaning compiled files..."
	@rm -f $(COMPILED)

clean-tests:
	@echo "Cleaning compiled test files..."
	@rm -f $(TEST_COMPILED)

test: compile
	@echo "Running tests..."
	@$(EMACS) -Q --batch $(LOADPATH) -l test/run-tests.el

test-only:
	@echo "Running tests (without recompiling)..."
	@$(EMACS) -Q --batch $(LOADPATH) -l test/run-tests.el
