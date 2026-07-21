UV ?= uv

.PHONY: sync test test-elisp check profiles

sync:
	$(UV) sync --extra test

test:
	$(UV) run pytest

test-elisp:
	$(MAKE) -C .. --no-print-directory test-benchmark

check: test test-elisp
	$(UV) run python -m compileall -q magent_benchmark

profiles:
	$(UV) run magent-bench profiles
