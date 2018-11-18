  λ(x : <>)
→ { bar = merge {=} x : Integer
  , foo =
      merge
      { Left = λ(b : Bool) → b, Right = Natural/even }
      < Left = True | Right : Natural >
  }
