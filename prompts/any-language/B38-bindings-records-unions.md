# B38 — Bindings: records and unions

Depends on: B37  
Read first: `prompts/any-language/00-shared.md`

## Records `{ x : T, y : U }`

Host struct/class/map with **exactly** those fields after normalization
(record field order in Dhall is sorted; host field names are the Dhall
labels). Extra Dhall fields → error. Missing fields → error. Nested
records recurse.

## Unions `< Left : A | Right : B | Empty >`

One active alternative. Empty alternatives are nullary (flag/enum).
Host encodings (tagged object, sealed class, ADT) are language-specific
but must be documented. Alternative names are Dhall labels.

## Tests (local)

- Decode `{ x = 1, y = True }` into a two-field struct.
- Reject `{ x = 1 }` for that decoder (missing `y`).
- Reject `{ x = 1, y = True, z = 0 }` (extra field).
- Decode `< Left : Natural | Right : Text >.Left 3`.
- Decode an empty alternative `< A | B >.A`.

## Done when

Those tests pass. Document the host union encoding in one paragraph.
