{ and =
      λ(xs : List Bool)
    → List/fold Bool xs Bool (λ(l : Bool) → λ(r : Bool) → l && r) True
, build =
      λ(f : ∀(bool : Type) → ∀(true : bool) → ∀(false : bool) → bool)
    → f Bool True False
, even =
      λ(xs : List Bool)
    → List/fold Bool xs Bool (λ(x : Bool) → λ(y : Bool) → x == y) True
, fold =
      λ(b : Bool)
    → λ(bool : Type)
    → λ(true : bool)
    → λ(false : bool)
    → if b then true else false
, not =
    λ(b : Bool) → b == False
, odd =
      λ(xs : List Bool)
    → List/fold Bool xs Bool (λ(x : Bool) → λ(y : Bool) → x != y) False
, or =
      λ(xs : List Bool)
    → List/fold Bool xs Bool (λ(l : Bool) → λ(r : Bool) → l || r) False
, show =
    λ(b : Bool) → if b then "True" else "False"
}
