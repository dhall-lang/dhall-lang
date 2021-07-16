{ and =
    λ(_ : List Bool) →
      List/fold Bool _ Bool (λ(_ : Bool) → λ(_ : Bool) → _@1 && _) True
, build = λ(_ : Type → _ → _@1 → _@2) → _ Bool True False
, even =
    λ(_ : List Bool) →
      List/fold Bool _ Bool (λ(_ : Bool) → λ(_ : Bool) → _@1 == _) True
, fold =
    λ(_ : Bool) → λ(_ : Type) → λ(_ : _) → λ(_ : _@1) → if _@3 then _@1 else _
, not = λ(_ : Bool) → _ == False
, odd =
    λ(_ : List Bool) →
      List/fold Bool _ Bool (λ(_ : Bool) → λ(_ : Bool) → _@1 != _) False
, or =
    λ(_ : List Bool) →
      List/fold Bool _ Bool (λ(_ : Bool) → λ(_ : Bool) → _@1 || _) False
, show = λ(_ : Bool) → if _ then "True" else "False"
}
