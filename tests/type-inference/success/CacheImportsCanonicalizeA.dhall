{-
	This URL returns (probably) a different result for each request. This test
	ensures that import locations are canonicalized before being cached.
-}
let _ =
		assert
	  :   https://test.dhall-lang.org/random-string as Text
		≡ https://test.dhall-lang.org/foo/../random-string as Text

in  0
