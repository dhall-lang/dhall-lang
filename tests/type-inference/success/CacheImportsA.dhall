{-
    This URL returns (probably) a different result for each request. This test
    ensures that import results for a given URL are correctly cached within an
    execution of dhall.
-}
let _ = assert : https://csrng.net/csrng/csrng.php?min=0&max=1000 as Text === https://csrng.net/csrng/csrng.php?min=0&max=1000 as Text in 0
