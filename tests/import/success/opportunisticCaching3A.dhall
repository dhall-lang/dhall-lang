{-
This test validates that opportunistic hashing works with env variable imports,
that it does not pick up the hash to the right of a successfully resolved alternative
(q ? missing sha256:x),
and also that an expression will not be cached when the provided hash does not match.

The test uses the hash 0000000000000000000000000000000000000000000000000000000011111111
that does not match the provided expression ../../tests/import/data/opportunistic.dhall
-}
{ case1 =
  [   missing
        sha256:c39cde2e11e3d5a57cccbc06f6599256ece67b3d16d1bc1df1d0cfa79d9be605
    ? 10
  ,   missing
        sha256:c39cde2e11e3d5a57cccbc06f6599256ece67b3d16d1bc1df1d0cfa79d9be605
    ? env:DHALL_TEST_VAR
  ,   missing
        sha256:c39cde2e11e3d5a57cccbc06f6599256ece67b3d16d1bc1df1d0cfa79d9be605
    ? 50
  ]
, case2 =
  [   missing
        sha256:0000000000000000000000000000000000000000000000000000000011111111
    ? 10
  ,   missing
        sha256:0000000000000000000000000000000000000000000000000000000011111111
    ? ../../import/data/opportunistic.dhall
  ,   missing
        sha256:0000000000000000000000000000000000000000000000000000000011111111
    ? 50
  ]
, case3 =
  [   missing
        sha256:2f811c3159958418bb9cb4089b5e89e68c92fafc1bb498f7bf89e4d644fe0ec3
    ? [ 10, 1 ]
  ,   ../../import/data/opportunistic1.dhall
    ? missing
        sha256:2f811c3159958418bb9cb4089b5e89e68c92fafc1bb498f7bf89e4d644fe0ec3
  ,   missing
        sha256:2f811c3159958418bb9cb4089b5e89e68c92fafc1bb498f7bf89e4d644fe0ec3
    ? [ 50, 1 ]
  ]
}
