{ ./some/import.dhall as Location
, ../data/simpleLocation.dhall
, /absolute/import as Location sha256:f9340badf94a684e652e0a384f64363293d8b632d971f3453f7ee22f10ab6e75
, https://prelude.dhall-lang.org/package.dhall as Location
, env:HOME as Location
, missing as Location
, (missing as Location) ? 42  -- `missing` fails as an import, but definitely resolves as Location
}
