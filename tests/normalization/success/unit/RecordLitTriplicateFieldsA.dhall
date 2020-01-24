{-  This test ensures that an implementation handles more than one duplicate
    field correctly and combines them with the correct order and associativity
-}
  λ(a : { x : Natural })
→ λ(b : { y : Natural })
→ λ(c : { z : Natural })
→ { k = a, k = b, k = c }
