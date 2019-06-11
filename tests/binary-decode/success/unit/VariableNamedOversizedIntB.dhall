{-

This file tests that a number encoded as a type larger than required is still
read successfully.  In this case, the .dhallb file has the bytes:

    8261 781b 0000 0000 0000 0001

which means the array ["x", uint64(1)].  This could have been stored in fewer
bytes as

    8261 7801

but decoders are required to allow the oversized representation.

-}
x@1
