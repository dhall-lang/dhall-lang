{-

This file tests that a number encoded as a type larger than required is still
read successfully.  In this case, the .dhallb file has the bytes:

    1b00 0000 0000 0000 01

which means a uint64 with value 1.  The number 1 could have been stored in a
single byte as 01, but decoders are required to allow the oversized
representation.

-}
_@1
