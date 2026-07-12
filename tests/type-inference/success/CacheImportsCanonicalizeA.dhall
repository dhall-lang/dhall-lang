{-
	This URL returns (probably) a different result for each request. This test
	ensures that import locations are canonicalized before being cached.
-}
let _ =
        assert
      :   https://localhost:18443/random-string as Text
        ≡ https://localhost:18443/foo/../random-string as Text

in  0
