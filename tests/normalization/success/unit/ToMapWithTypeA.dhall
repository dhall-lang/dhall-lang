{- This test verifies that the normalized result does not contain a type
   annotation if the input record is non-empty.

   The reason for this test is that implementations might need to store
   optional type annotations for both `toMap` invocations and empty list
   literals.  If an implementation does so, a common mistake is to take
   the optional type annotation and always pass it through as the optional
   type annotation for the list literal that `toMap` generates.

   However, the standard specifies that `toMap` should only generate a
   type annotation if the input record is empty.  If the input record is
   non-empty then the resulting normalized list is non-empty and therefore
   should not have a type annotation (since a non-empty list with a type
   annotation is not in normal form).
-}
toMap { foo= 1, bar= 4, baz= 9 } : List { mapKey : Text, mapValue : Natural }
