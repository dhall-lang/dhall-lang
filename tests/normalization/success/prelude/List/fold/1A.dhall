  λ(nil : Natural)
→ (../../../../../Prelude/package.dhall).`List`.fold
  Natural
  [ 2, 3, 5 ]
  Natural
  (λ(x : Natural) → λ(y : Natural) → x + y)
  nil
