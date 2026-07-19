{-
Validate the caching behavior: if an import is hash-protected then it will be cached on first use.
Subsequent reads will retrieve that expression from the cache.

This test first verifies that the value in simple.dhall is not yet cached (step1).
Then it imports that expression while specifying the hash (step2).
This should write the expression to the cache under the provided hash.

Finally we verify that the cache product can be loaded if that hash is specified (step3).

To be sure that import caching is really based on the file cache, we use different
nonexistent imports (nonexistent-file-1, 2).
../../data/simple.dhall
-}
{ step1 =
      ../../data/nonexistent-file-1.dhall
        sha256:15f52ecf91c94c1baac02d5a4964b2ed8fa401641a2c8a95e8306ec7c1e3b8d2
    ? 10
, step2 =
    ../../import/data/simple.dhall
      sha256:15f52ecf91c94c1baac02d5a4964b2ed8fa401641a2c8a95e8306ec7c1e3b8d2
, step3 =
      ../../data/nonexistent-file-2.dhall
        sha256:15f52ecf91c94c1baac02d5a4964b2ed8fa401641a2c8a95e8306ec7c1e3b8d2
    ? 50
}
