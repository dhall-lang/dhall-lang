{- The language standard used to require that custom headers were provided via
   an external import, but was later amended to support inline headers as part
   of:

   https://github.com/dhall-lang/dhall-lang/pull/560

   This test verifies that an implementation supports such inline custom headers
-}

https://example.com/foo using
    [ { mapKey   = "Authorization"
      , mapValue = "token 5199831f4dd3b79e7c5b7e0ebe75d67aa66e79d4"
      }
    ]
