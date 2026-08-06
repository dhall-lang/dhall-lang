{-
This test validates the associativity of ? in the presence of opportunistic caching.
It is validated that the hash is picked up from any part of the import expression
to the left of the successful import.

The test resolves the following expressions and checks that the cache product for r
was not present in the cache before evaluation but is present afterwards (except for 5, 6, 7):

1) p sha256:x ? (q ? r)

2) p sha256:x ? (r ? q)

3) (p sha256:x ? q) ? r

4) (q ? p sha256:x) ? r

5) (q ? r) ? p sha256:x   -- r is not written

6) (r ? q) ? p sha256:x   -- r is not written

7) (q sha256:y ? p sha256:x) ? r   -- r is not written

8) (p sha256:x ? q sha256:y) ? r

Here p and q are missing but r resolves successfully and has the provided hash (the "x" in "sha256:x").
The hash "y" is intentionally different from "x".

The test validates that the successfully resolved r (which is itself not hash-protected)
will be nevertheless "opportunistically" cached under the hash specified to the left
of r within a hash-protected but missing import.

The cases "p sha256:x ? r", "p sha256:y ? r", and "r ? p sha256:x"
have been validated in previous tests.
-}
{ case1 =
  [   ../../import/data/nonexistent-file-1
        sha256:2f811c3159958418bb9cb4089b5e89e68c92fafc1bb498f7bf89e4d644fe0ec3
    ? [ 10, 1 ]
  ,   ../../import/data/nonexistent-file
        sha256:2f811c3159958418bb9cb4089b5e89e68c92fafc1bb498f7bf89e4d644fe0ec3
    ? (   ../../import/data/opportunistic1.dhall
        ? ../../import/data/nonexistent-file-2
      )
  ,   missing
        sha256:2f811c3159958418bb9cb4089b5e89e68c92fafc1bb498f7bf89e4d644fe0ec3
    ? [ 50, 1 ]
  ]
, case2 =
  [   ../../import/data/nonexistent-file-1
        sha256:f5e4c7bc7f161fdd15d9afbed6a9f46fc398960edb599f9c940ff414553536c1
    ? [ 10, 2 ]
  ,   missing
        sha256:f5e4c7bc7f161fdd15d9afbed6a9f46fc398960edb599f9c940ff414553536c1
    ? (   ../../import/data/nonexistent-file-2
        ? ../../import/data/opportunistic2.dhall
      )
  ,   missing
        sha256:f5e4c7bc7f161fdd15d9afbed6a9f46fc398960edb599f9c940ff414553536c1
    ? [ 50, 2 ]
  ]
, case3 =
  [   ../../import/data/nonexistent-file-1
        sha256:bbf55e7e3f3be9684ea471322065dc5f6355badf7eacb75509890beca97a6b05
    ? [ 10, 3 ]
  ,   ../../import/data/nonexistent-file
        sha256:bbf55e7e3f3be9684ea471322065dc5f6355badf7eacb75509890beca97a6b05
    ? ../../import/data/nonexistent-file-2
    ? ../../import/data/opportunistic3.dhall
  ,   missing
        sha256:bbf55e7e3f3be9684ea471322065dc5f6355badf7eacb75509890beca97a6b05
    ? [ 50, 3 ]
  ]
, case4 =
  [   ../../import/data/nonexistent-file-1
        sha256:419202f2e8ce9519f468ffaa6422add7171426f50732c3c77e281fb8cf2e32c7
    ? [ 10, 4 ]
  ,   ../../import/data/nonexistent-file-2
    ? missing
        sha256:419202f2e8ce9519f468ffaa6422add7171426f50732c3c77e281fb8cf2e32c7
    ? ../../import/data/opportunistic4.dhall
  ,   missing
        sha256:419202f2e8ce9519f468ffaa6422add7171426f50732c3c77e281fb8cf2e32c7
    ? [ 50, 4 ]
  ]
}
