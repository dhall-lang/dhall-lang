  λ(x : <>)
→ { example1 = merge {=} x : Integer
  , example2 =
	  merge
	  { Left = λ(b : Bool) → b, Right = Natural/even }
	  (< Left : Bool | Right : Natural >.Left True)
  , example3 = merge { _ = True } x : Bool
  }
