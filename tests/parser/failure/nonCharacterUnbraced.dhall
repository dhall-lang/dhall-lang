{-
The parser should reject unicode escape sequences for non-characters
(0xNFFFE-0xNFFFF for each `N` in `{ 0 .. F }), regardless of whether they are
braced or not.
-}
"\uFFFE"
