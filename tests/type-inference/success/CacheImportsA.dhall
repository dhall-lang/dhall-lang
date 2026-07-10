{-
	This URL returns (probably) a different result for each request. This test
	ensures that import results for a given URL are correctly cached within an
	execution of dhall.
-}
let _ =
		assert
	  :   http://localhost:18080/random-string as Text
		≡ http://localhost:18080/random-string as Text

in  0
