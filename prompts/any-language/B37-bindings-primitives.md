# B37 — Bindings: primitives and collections

Depends on: B36  
Read first: `prompts/any-language/00-shared.md`

## Mapping

| Dhall | Host | Notes |
|---|---|---|
| Bool | boolean | |
| Text | string / unicode text | UTF-8; not JSON |
| Bytes | byte array | |
| Natural | unbounded unsigned integer | no silent truncate |
| Integer | unbounded signed integer | |
| Double | IEEE binary64 | preserve NaN/-0.0 if the host can |
| Date | civil date (Y-M-D) | |
| Time | time-of-day + **precision** | precision is part of the value |
| TimeZone | minutes offset | |
| List a | sequence of a | empty list still has a type |
| Optional a | option/nullable | `None` needs type argument `A` |

## Tests (local)

- Round-trip Natural `2^64` (or skip with an explicit error if the host
  cannot, and document that as a **documented deviation**).
- `[] : List Bool` → empty list, not a type error.
- `None Bool` → empty optional; `None` without argument fails.
- `Some True` → optional of true.

## Done when

Primitive + List + Optional decoders/encoders work on those tests.
