UV ?= uv
CONFIG ?= config.toml

.DEFAULT_GOAL := prepare

.PHONY: prepare prepare-emacs bench sync test test-elisp check profiles

# Normal workflow: edit config.toml, run `make`, then run `make bench`.
prepare: sync check prepare-emacs
	$(UV) run magent-bench prepare --config $(CONFIG)

prepare-emacs:
	$(UV) run magent-bench prepare-emacs --config $(CONFIG)

bench:
	$(UV) run magent-bench bench --config $(CONFIG)

sync:
	$(UV) sync --extra test

test:
	$(UV) run pytest

test-elisp:
	$(MAKE) -C .. --no-print-directory test-benchmark

check: test test-elisp
	$(UV) run python -m compileall -q magent_benchmark

profiles:
	$(UV) run magent-bench profiles --config $(CONFIG)
