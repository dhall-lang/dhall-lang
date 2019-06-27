{- This file is provided for backwards compatibility since `./Map.dhall`
   was relocated to `./Map/Type`

   The main reason for standardizing on a `./*/Type` convention is so that
   there can eventually be a one-to-one correspondence between Prelude files and
   the fields of the `package.dhall` Prelude record once we can store types
   within that record.  So, for example, there will eventually be a
   `Prelude.Map.Type` field corresponding to `./Prelude/Map/Type`.

   At some point we will remove this file after a sufficiently long
   deprecation cycle.
-}
  ./Map/Type sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
? ./Map/Type
