{-| Transforms a function of type T → R into an equivalent function
that does not expand its argument of type T.

To use, provide a predicate (T → Bool) that always returns True but such that Dhall
cannot statically recognize that property when reducing that predicate under a lambda.

Examples of such predicates are available for Bool, Integer, Natural, Optional, and List input types.

Also provide a default value of the output type (R). That value will be never used in actual evaluation.

Example usage:

Suppose we want to prevent the standard function `List/index` from expanding its first argument (of type Natural).
We define a function `listIndexNonExpanding` with the same type signature as `List/index` like this:

```
let listIndexNonExpanding : Natural → ∀(a : Type) → List a → Optional a
  = let R = ∀(a : Type) → List a → Optional a
    let defaultR : R = λ(a : Type) → λ(_ : List a) → None a
    in Function/nonexpanding Natural Function/nonexpandingPredicate.Natural R defaultR List/index
```
-}
let nonexpanding
    : ∀(T : Type) → (T → Bool) → ∀(R : Type) → R → (T → R) → T → R
    = λ(T : Type) →
      λ(predicate : T → Bool) →
      λ(R : Type) →
      λ(default : R) →
      λ(f : T → R) →
      λ(x : T) →
        merge
          { Some = f, None = default }
          (if predicate x then Some x else None T)

in  nonexpanding
