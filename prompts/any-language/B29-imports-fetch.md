# B29 — Imports: fetch Code/Text/Bytes, env, missing, alternatives

Depends on: B28, B17, B26  
Read first: `prompts/any-language/00-shared.md`

## Spec

Notes: fetch by mode; soft vs hard `?` table; hash of Text/Bytes is hash
of **encoded literals**. Code: parse → resolve children → type-check empty
Γ → β-normalize. Cycles are hard failures.

## Tests that must pass

Local/env success: `Env*`, `AsText`, `AsBytes`, `Missing`, `Alternative*`,
`Simple`, `MixImportModes`, `DontTryResolving`, `Normalize`.

Failure: `EnvUnset*`, `FileMissing`, `Missing`, `Cycle`,
`DontRecoverCycle`, `DontRecoverParseError`, `DontRecoverTypeError`,
`VarAcrossImportBoundary`, `alternativeEnv*`.

Honor `*ENV.dhall` sidecars.

## Done when

Listed tests pass. Skip HTTP/hash/CORS.
