{- Fragment identifiers are not allowed in URLs because they serve no purpose
   for Dhall and they could lead to ambiguity if a parser interprets them as
   the list append operator (`#`)

   The following expression therefore only has one valid parse, which is to
   interpret the `#` as a list append.  In other words, the following expression
   is parsed as:

   (https://example.com/foo) # bar
-}
https://example.com/foo#bar
