{- This test checks that an implementation doesn't mistakenly implement the
   optimization suggested by the standard without the requisite shift
   judgment
-}
let _ = 1 in {=} with a = _
