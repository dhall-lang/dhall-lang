{- This is a type error because the custom headers are type-checked with an
   empty context.  This is necessary because:

   * import resolution precedes β-normalization
   * we also don't want custom headers to leak program state anyway

   This should fail due to the `x` within the custom header being an unbound
   variable
-}

let x = "Bar"

in http://localhost:18080/headers
     using [ { mapKey = "Foo", mapValue = x } ]
     as Text
