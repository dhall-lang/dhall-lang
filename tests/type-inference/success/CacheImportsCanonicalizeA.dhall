{-
	This URL returns (probably) a different result for each request. This test
	ensures that import locations are canonicalized before being cached.
-}
let _ =
		assert
	  :   http://localhost:18080/random-string as Text
		≡ http://localhost:18080/foo/../random-string as Text

in  0
