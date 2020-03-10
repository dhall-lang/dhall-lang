{- This is a "referentially opaque" import (i.e. an import that is not
   globally addressable), which cannot be imported by a "referentially
   transparent" import (i.e. an import that is globally addressable).

   This test file is used in a failing test to verify that referentially
   transparent imports cannot import referentially opaque imports.  In the test
   suite this file is actually imported via its GitHub URL (not its local file
   path), so it plays the role of the referentially transparent import.  Then,
   this file attempts to import a referentially opaque import (an environment
   variable in this case) to verify that the import fails.

   For this test file we need to select a referentially opaque import that
   would succeed if imported on its own, so that a non-compliant implementation
   doesn't fail this test for the wrong reason (i.e. due to the import not
   being present).  We thus use the `DHALL_TEST_VAR` environment variable, that
   is expected to be set when running import tests.
-}
env:DHALL_TEST_VAR as Text
