{-
The parser should reject unicode escape sequences for surrogate pairs
(0xD800-DFFF), regardless of whether they are braced or not.
-}
"\uD800"
