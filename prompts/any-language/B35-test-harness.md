# B35 — Test harness completeness

Depends on: B32, B10, B08  
Read first: `prompts/any-language/00-shared.md`

## Goal

Mirror `tests/README.md` exactly: parser, binary-decode, α, β,
type-inference (timeouts), semantic-hash, import (server, HOME, cache,
ENV sidecars).

## Tests that must pass

Every file the README describes. No skipped suites without a listed
reason (only missing as-Source fixtures if the standard PR is not
merged).

## Done when

A single command runs the full suite and exits 0.
