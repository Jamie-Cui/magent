EMACS ?= emacs
EMACSCLIENT ?= emacsclient
EMACS_BATCH = $(EMACS) -Q --batch
UV ?= uv
DOCKER ?= docker
COVERAGE_DIR ?= coverage
COVERAGE_MIN ?= 65
SOURCE_MANIFEST ?= source-files.txt
BENCHMARK_CONFIG ?= $(abspath benchmark/config.toml)
HARBOR_CACHE = $(HOME)/.cache/harbor

# Auto-detect dependency paths
elpa-package-dir = $(shell found=; \
	for dir in "$(HOME)"/.emacs.d/elpa/$(1); do \
		if [ -d "$$dir" ]; then found=$$dir; fi; \
	done; \
	if [ -n "$$found" ]; then printf '%s\n' "$$found"; fi)

GPTEL_DIR ?= $(call elpa-package-dir,gptel-[0-9]*)
TRANSIENT_DIR ?= $(call elpa-package-dir,transient-*)
ACP_DIR ?= $(call elpa-package-dir,acp-*)
SHELL_MAKER_DIR ?= $(call elpa-package-dir,shell-maker-*)
AGENT_SHELL_DIR ?= $(call elpa-package-dir,agent-shell-*)
COND_LET_DIR ?= $(call elpa-package-dir,cond-let-*)
COMPAT_DIR ?= $(call elpa-package-dir,compat-*)
YAML_DIR ?= $(call elpa-package-dir,yaml-[0-9]*)
LLAMA_DIR ?= $(call elpa-package-dir,llama-*)
WITH_EDITOR_DIR ?= $(call elpa-package-dir,with-editor-*)

LOADPATH = -L lisp \
	$(if $(GPTEL_DIR),-L "$(GPTEL_DIR)") \
	$(if $(TRANSIENT_DIR),-L "$(TRANSIENT_DIR)") \
	$(if $(ACP_DIR),-L "$(ACP_DIR)") \
	$(if $(SHELL_MAKER_DIR),-L "$(SHELL_MAKER_DIR)") \
	$(if $(AGENT_SHELL_DIR),-L "$(AGENT_SHELL_DIR)") \
	$(if $(COND_LET_DIR),-L "$(COND_LET_DIR)") \
	$(if $(COMPAT_DIR),-L "$(COMPAT_DIR)") \
	$(if $(YAML_DIR),-L "$(YAML_DIR)") \
	$(if $(LLAMA_DIR),-L "$(LLAMA_DIR)") \
	$(if $(WITH_EDITOR_DIR),-L "$(WITH_EDITOR_DIR)")

# Compilation flags
BYTE_COMPILE_FLAGS = --eval "(setq byte-compile-error-on-warn nil)" \
	--eval "(setq byte-compile-warnings '(not cl-functions))"

SRCS = $(shell sed -e '/^[[:space:]]*\#/d' -e '/^[[:space:]]*$$/d' "$(SOURCE_MANIFEST)")

COMPILED = $(SRCS:.el=.elc)

.PHONY: all compile clean purge test test-unit test-benchmark test-live test-live-smoke coverage help \
	benchmark benchmark-prepare benchmark-run benchmark-test

all: compile

help:
	@echo "Magent"
	@echo ""
	@echo "Development targets:"
	@echo "  make          - Default; same as make compile"
	@echo "  compile       - Byte compile all Elisp files"
	@echo "  clean         - Remove build files and Harbor trial containers/networks"
	@echo "  purge         - Clean plus benchmark Docker images and Harbor task cache"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Test targets:"
	@echo "  test            - Run unit tests and deterministic live smoke tests"
	@echo "  test-unit       - Run batch unit tests"
	@echo "  test-benchmark  - Run deterministic benchmark adapter tests"
	@echo "  test-live       - Run real live gptel tests; consumes tokens and requires Emacs server"
	@echo "  test-live-smoke - Run live Emacs smoke tests with stubbed gptel transport"
	@echo "  coverage        - Run ERT under built-in testcover"
	@echo ""
	@echo "Benchmark targets:"
	@echo "  benchmark         - Prepare when needed, then run the benchmark"
	@echo "  benchmark-prepare - Test and prepare without running the benchmark"
	@echo "  benchmark-run     - Run an already prepared benchmark"
	@echo "  benchmark-test    - Run all deterministic benchmark tests"

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
	@$(EMACS_BATCH) $(LOADPATH) \
		-l ert \
		-l test/magent-test.el \
		-f ert-run-tests-batch-and-exit

test-benchmark:
	@echo "Running benchmark adapter tests..."
	@$(EMACS_BATCH) $(LOADPATH) \
		-l benchmark/tests/runner-test.el \
		-f ert-run-tests-batch-and-exit

benchmark:
	@config="$(BENCHMARK_CONFIG)"; \
	prepared="$$(dirname "$$config")/generated/benchmark.yaml"; \
	if [ ! -f "$$prepared" ] || [ "$$config" -nt "$$prepared" ]; then \
		$(MAKE) --no-print-directory benchmark-prepare; \
	fi
	@$(MAKE) --no-print-directory benchmark-run

benchmark-prepare: benchmark-test
	$(UV) --directory benchmark run --locked magent-bench prepare-emacs --config "$(BENCHMARK_CONFIG)"
	$(UV) --directory benchmark run --locked magent-bench prepare --config "$(BENCHMARK_CONFIG)"

benchmark-run:
	$(UV) --directory benchmark run --locked magent-bench bench --config "$(BENCHMARK_CONFIG)"

benchmark-test: test-benchmark
	$(UV) --directory benchmark run --locked --extra test pytest

test-live:
	@echo "Running real live Emacs/gptel tests..."
	@$(EMACSCLIENT) -e "(progn (load-file \"$(CURDIR)/test/magent-live-test.el\") (magent-live-test-run))"

test-live-smoke:
	@echo "Running live Emacs smoke tests..."
	@$(EMACSCLIENT) -e "(progn (load-file \"$(CURDIR)/test/magent-live-test.el\") (magent-live-test-run-smoke))"

coverage:
	@echo "Running coverage..."
	@$(EMACS_BATCH) $(LOADPATH) \
		--eval "(setq magent-coverage-directory \"$(COVERAGE_DIR)\" magent-coverage-min $(COVERAGE_MIN))" \
		-l test/coverage.el

clean:
	@echo "Cleaning Harbor benchmark containers and networks..."
	@set -e; \
	if command -v "$(DOCKER)" >/dev/null 2>&1 && $(DOCKER) info >/dev/null 2>&1; then \
		$(DOCKER) ps -a --format '{{.ID}} {{.Names}}' | \
		while read -r id name; do \
			case "$$name" in \
				*__env-main-1) \
					echo "Removing benchmark container $$name"; \
					$(DOCKER) rm -f "$$id" >/dev/null ;; \
			esac; \
		done; \
		$(DOCKER) network ls --format '{{.ID}} {{.Name}}' | \
		while read -r id name; do \
			case "$$name" in \
				*__env_default) \
					echo "Removing benchmark network $$name"; \
					$(DOCKER) network rm "$$id" >/dev/null ;; \
			esac; \
		done; \
	else \
		echo "Docker unavailable; skipping Harbor container and network cleanup"; \
	fi
	@echo "Cleaning compiled and rebuildable benchmark files..."
	@rm -f lisp/*.elc
	@rm -rf benchmark/.venv \
		benchmark/generated \
		benchmark/runtime \
		benchmark/.pytest_cache \
		benchmark/magent_benchmark.egg-info \
		benchmark/magent_benchmark/__pycache__ \
		benchmark/tests/__pycache__
	@rm -f benchmark/elisp/*.elc benchmark/tests/*.elc

purge: clean
	@echo "Purging benchmark Docker images..."
	@set -e; \
	if command -v "$(DOCKER)" >/dev/null 2>&1 && $(DOCKER) info >/dev/null 2>&1; then \
		$(DOCKER) image ls --format '{{.ID}} {{.Repository}}' | \
		while read -r id repository; do \
			case "$$repository" in \
				*__env-main|hb__*|sb__*|swebench/sweb.eval.*) \
					echo "Removing benchmark image $$repository"; \
					$(DOCKER) image rm -f "$$id" >/dev/null ;; \
			esac; \
		done; \
	else \
		echo "Docker unavailable; skipping benchmark image cleanup"; \
	fi
	@cache="$(HARBOR_CACHE)"; \
	case "$$cache" in \
		*/.cache/harbor) \
			echo "Removing Harbor task cache $$cache"; \
			rm -rf "$$cache" ;; \
		*) \
			echo "Refusing unsafe Harbor cache path: $$cache" >&2; \
			exit 1 ;; \
	esac
