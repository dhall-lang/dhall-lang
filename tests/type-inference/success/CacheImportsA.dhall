{-
	This URL returns (probably) a different result for each request. This test
	ensures that import results for a given URL are correctly cached within an
	execution of dhall.
-}
let _ =
		assert
	  :   https://test.dhall-lang.org/random-string as Text
		≡ https://test.dhall-lang.org/random-string as Text

in  0
