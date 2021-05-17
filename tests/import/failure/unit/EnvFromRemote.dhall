{- The following remote import attempts to import an environment variable, which
   must be disallowed by the referential sanity check

   One reason for doing this is to protect against remote imports exfiltrating
   environment variables (such as via custom headers).  Only referentially
   opaque imports (i.e. local imports) have permission to refer to other
   referentially opaque imports in order to protect against this attack.

   The referential sanity check also ensures that remote imports are
   referentially transparent.  Or in other words, any import that is globally
   addressable must have a meaning that is not context-sensitive.
-}
https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/tests/import/data/referentiallyOpaque.dhall
