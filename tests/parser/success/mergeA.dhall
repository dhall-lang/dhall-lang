    \(x : <>)
->  { bar = merge {=} x : Integer
    , foo =
        merge
          { Left = \(b : Bool) -> b, Right = Natural/even }
          (< Left : Bool | Right : Natural >.Left True)
    , baz = merge { _ = True } x : Bool
    }
