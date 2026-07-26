# Magent Model Inheritance and Proxy Error Design

## Problem

Magent currently turns an agent definition with no model override into an
effective `nil` `gptel-model`.  In Emacs Lisp, `nil` satisfies `symbolp`, so
`magent-agent-info-apply-gptel-overrides` treats absence as an explicit symbol
override instead of inheriting `(default-value 'gptel-model)`.

With the configured DeepSeek backend, gptel sanitizes that unsupported `nil`
model to the backend's first registered model, `deepseek-reasoner`.  The active
DeepSeek API accepts `deepseek-v4-pro` and `deepseek-v4-flash`, so it returns
HTTP 400.

When gptel uses an HTTP proxy, curl emits the proxy CONNECT response headers
before the origin response headers.  gptel records the CONNECT 200 as the
request status, while the origin HTTP/2 400 and JSON error remain in the
apparent response body.  The streaming cleanup therefore reports successful
completion without content, and agent-shell displays no reply.

## Goals

- A Magent agent with no model override inherits the default gptel model.
- Explicit symbol, string, backend, and `(BACKEND . MODEL)` overrides retain
  their documented behavior.
- Magent-managed proxied curl requests suppress CONNECT response headers so
  gptel observes the origin HTTP status and provider error.
- Non-Magent gptel requests and non-proxied Magent requests remain unchanged.
- Repeated Magent initialization does not install duplicate advice or duplicate
  curl arguments.
- The fix is covered by focused regression tests and verified in live Emacs.

## Non-Goals

- Rewriting gptel provider, HTTP, curl, or SSE plumbing.
- Modifying the installed third-party gptel package.
- Changing global user gptel configuration.
- Adding retry or fallback-model behavior.
- Changing session persistence, ledger schema, frontend protocol, or provider
  model registration.

## Design

### Model Inheritance

`magent-agent-info-apply-gptel-overrides` will distinguish absence from an
explicit symbol before the general `symbolp` branch:

1. A cons whose cdr is a symbol selects that model.
2. `nil` selects `(default-value 'gptel-model)`.
3. A non-nil symbol selects itself.
4. A string is interned.
5. Other values fall back to `(default-value 'gptel-model)`.

This implements the function's existing documented contract.  No compatibility
bridge or data migration is needed.

### Proxy CONNECT Header Suppression

`magent-llm-gptel.el` will add one adapter-local around advice for
`gptel-curl--get-config-args`, alongside the existing managed gptel boundary
advice.

The advice will:

- call the original gptel function first;
- inspect the request `info` with `magent-llm-gptel--managed-info-p`;
- when the request is Magent-managed and `gptel-proxy` is a non-empty string,
  append `--suppress-connect-headers` unless already present;
- otherwise return the original argument list unchanged.

This keeps provider transport in `gptel-request` and changes only curl
presentation of proxy CONNECT headers for Magent-managed requests.  It avoids
global `gptel-curl-extra-args` mutation and does not reparse provider responses
inside Magent.

The installed curl 8.7.1 supports `--suppress-connect-headers`.  The option is
specifically designed to omit proxy CONNECT response headers while preserving
the origin response headers consumed by gptel.

## Data and Error Flow

After the change:

1. The built-in `build` agent has no model override.
2. Magent inherits `deepseek-v4-pro` from the default gptel model.
3. The Magent adapter starts the same `gptel-request` transport as before.
4. With a proxy configured, curl omits only the CONNECT response headers.
5. gptel parses the origin HTTP status and either streams a normal completion
   or invokes the error callback with the provider error.
6. Magent projects successful text or a real error through its existing
   runtime and ACP lifecycle.

No session data is rewritten or migrated.

## Testing

Focused ERT tests will be written before implementation and observed failing:

- a nil agent model inherits the default gptel model;
- an explicit symbol model still overrides the default;
- a Magent-managed proxied request gains exactly one
  `--suppress-connect-headers` argument;
- a managed request without a proxy is unchanged;
- a non-Magent proxied request is unchanged;
- installing the boundary advice repeatedly remains idempotent.

After the focused tests pass, verification will run:

1. the targeted ERT selector for the new regression tests;
2. `make compile`;
3. `make test-unit`;
4. live Emacs reload of the two changed source files;
5. a non-tool prompt (`你好`);
6. a tool prompt (`帮我看下 emacs 里面有多少 buffer`);
7. inspection of `*magent-log*`, `*Messages*`, and the active agent-shell
   request result.

## Compatibility and Breaking-Change Policy

The session-wide breaking-change policy is active, but this design does not
need to break a project-owned interface.  It restores the current documented
nil-inheritance contract and hardens an internal gptel adapter boundary.
External gptel APIs remain authoritative, and no compatibility shim is added.

## Risks

- `gptel-curl--get-config-args` is a gptel-private function.  Magent already
  confines gptel-private integration to `magent-llm-gptel.el`; focused advice
  installation and behavior tests will detect signature or lifecycle drift.
- A future curl lacking `--suppress-connect-headers` would fail the request.
  The current runtime uses curl 8.7.1, and the option has been available in
  supported curl releases well before this environment.  Magent will not add a
  speculative version-detection layer.
