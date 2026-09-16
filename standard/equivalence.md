# Equivalence

```haskell
module Equivalence where

import AlphaNormalization (alphaNormalize)
import BetaNormalization (betaNormalize)
import Binary (encode)
import Syntax (Expression)

import qualified Codec.CBOR.Term  as Term
import qualified Codec.CBOR.Write as Write
```

Equivalence is a relationship between two expression of the form:

    l ≡ r


... where:


* `l` (an input) is an expression to test for equivalence to `r`
* `r` (an input) is an expression to test for equivalence to `l`

```haskell
equivalent :: Expression -> Expression -> Bool
```

Two expressions are equivalent if they are identical after β-normalization,
α-normalization, and binary encoding:


    l₀ ⇥ l₁   l₁ ↦ x   encode(x) = b   r₀ ⇥ r₁   r₁ ↦ y   encode(y) = b
    ───────────────────────────────────────────────────────────────────
    l₀ ≡ r₀


```haskell
equivalent l₀ r₀ = encoded x == encoded y
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀

    x = alphaNormalize l₁
    y = alphaNormalize r₁

    encoded expression =
        Write.toStrictByteString (Term.encodeTerm (encode expression))
```

Note that this definition of equivalence does not include η-equivalence, so
`λ(f : Bool → Bool) → λ(x : Bool) → f x` and `λ(f : Bool → Bool) → f` are not
equivalent.

Note also that this means that `Double`s should not be compared using standard float equality.
