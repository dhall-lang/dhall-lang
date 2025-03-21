{-| The "non-expanding predicates" defined below have type T → Bool and always return True.
However, Dhall cannot statically recognize that property when reducing that predicate under a lambda.

This module has non-expanding predicates of type T → Bool defined for T = Bool,
T = Integer, T = Natural, as well as T = List A and T = Optional A for any type parameter A.

For example, this Dhall function:
```
λ(ot : Optional Text) → predicateOptional Text ot
```
is a function of type `Optional Text → Bool` that in fact always returns `True`.
However, the normal form of this function is not just `λ(ot : Optional Text) → True` because Dhall cannot detect this behavior statically.

Starting from these predicates, it is straightforward to construct non-expanding predicates for any other types.
-}
let predicateNatural
    : Natural → Bool
    = λ(x : Natural) →
        Natural/isZero (Natural/subtract 1 (Natural/subtract x 1))

let predicateBool
    : Bool → Bool
    = λ(x : Bool) → predicateNatural (if x then 1 else 0)

let predicateInteger
    : Integer → Bool
    = λ(x : Integer) → predicateNatural (Integer/clamp x)

let predicateList
    : ∀(T : Type) → List T → Bool
    = λ(T : Type) → λ(list : List T) → predicateNatural (List/length T list)

let predicateOptional
    : ∀(T : Type) → Optional T → Bool
    = λ(T : Type) →
      λ(ot : Optional T) →
        predicateNatural (merge { Some = λ(_ : T) → 1, None = 0 } ot)

in  { Bool = predicateBool
    , Integer = predicateInteger
    , Natural = predicateNatural
    , List = predicateList
    , Optional = predicateOptional
    }
