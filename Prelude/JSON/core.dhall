{-|
A record of functions useful for constructing `JSON` values.

This is only a subset of what `package.dhall` exports. If you are not writing a
JSON prelude function, you should use the `package.dhall` file instead.

It is used internally by `render`, `renderYAML` and `omitNullFields` instead of
`package.dhall` to avoid import cycles.
-}
{ Type =
      ./Type.dhall
        sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
    ? ./Type.dhall
, Tagged =
      ./Tagged.dhall
        sha256:21feca7d2b23f210d0696131d792e18a7d24fdcc85d41a49ba85b98670eba194
    ? ./Tagged.dhall
, Nesting =
      ./Nesting.dhall
        sha256:6284802edd41d5d725aa1ec7687e614e21ad1be7e14dd10996bfa9625105c335
    ? ./Nesting.dhall
, keyText =
      ./keyText.dhall
        sha256:f7b6c802ca5764d03d5e9a6e48d9cb167c01392f775d9c2c87b83cdaa60ea0cc
    ? ./keyText.dhall
, keyValue =
      ./keyValue.dhall
        sha256:a0a97199d280c4cce72ffcbbf93b7ceda0a569cf4d173ac98e0aaaa78034b98c
    ? ./keyValue.dhall
, string =
      ./string.dhall
        sha256:7ddb3a3b9f3ed09ed011d621a10ad9825185cd03503be98a81d42f6afb77940e
    ? ./string.dhall
, number =
      ./number.dhall
        sha256:e70162c73c4978ad0d0d99505f61c7d990f3abadfcc08b34388b29c0934a7a32
    ? ./number.dhall
, double =
      ./double.dhall
        sha256:e70162c73c4978ad0d0d99505f61c7d990f3abadfcc08b34388b29c0934a7a32
    ? ./double.dhall
, integer =
      ./integer.dhall
        sha256:c81a417397fc6f62155ec71fdd8d2047f981f0881295b307de3dd88747bf7e40
    ? ./integer.dhall
, natural =
      ./natural.dhall
        sha256:a839dc6789f19f820e9cbf70c60f41f3b057c59ece1d226d04db5aca447eb0e5
    ? ./natural.dhall
, object =
      ./object.dhall
        sha256:869aede785c34798be9f9fd457ece73e7f683f352ae4555f791516a365faf4ac
    ? ./object.dhall
, array =
      ./array.dhall
        sha256:fb6346a9c63638fe3c59f8108e19eebdbddc51389ec5570bab4c25f890ccccc8
    ? ./array.dhall
, bool =
      ./bool.dhall
        sha256:e043d9ed01e5b45899059e128243f3dae7ce65f293f0015ce816fc36d30f7f39
    ? ./bool.dhall
, null =
      ./null.dhall
        sha256:1eeb9aee38eb8dde0e64efbaf60f24612c8194cc00b510bfb627c2ee2e1877b8
    ? ./null.dhall
, renderInteger =
      ./renderInteger.dhall
        sha256:15b8d2ae46d5002832741927af787761df49798c911e2bf85db7a7b9cb5c078c
    ? ./renderInteger.dhall
}
