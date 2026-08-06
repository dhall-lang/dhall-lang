{-
 Validate the opportunistic caching in https://github.com/dhall-lang/dhall-haskell/pull/2781.
 In an import alternative, whenever an expression happens to have the same hash as
 the previous alternative, that expression will be cached under that hash.

For example, `missing sha256:abc123... ? p` will opportunistically cache `p`
under the hash `abc123...` in case `p`'s content matches that hash.

Subsequent reads will retrieve that expression from the cache.

In this test, opportunistic.dhall should have the value 123.

This test first verifies that the value in opportunistic.dhall is not yet cached (step1).
Then it imports that expression while specifying the hash in another alternative (step2).
This should write the expression 123 to the cache under the provided hash ("ec003...").

Finally we verify that the cache product can be loaded if that hash is specified (step3).

To be sure that import caching is really based on the file cache, we use three different
nonexistent imports (nonexistent-file-1, 2, 3).
-}
{ step1 =
      ../../data/nonexistent-file-1.dhall
        sha256:ec003014f4b86f39363c49d099db0547130af68f7f59fa78f4bce3a559b3e48c
    ? 10
, step2 =
      ../../data/nonexistent-file-2.dhall
        sha256:ec003014f4b86f39363c49d099db0547130af68f7f59fa78f4bce3a559b3e48c
    ? ../../import/data/opportunistic.dhall
, step3 =
      ../../data/nonexistent-file-3.dhall
        sha256:ec003014f4b86f39363c49d099db0547130af68f7f59fa78f4bce3a559b3e48c
    ? 50
}
