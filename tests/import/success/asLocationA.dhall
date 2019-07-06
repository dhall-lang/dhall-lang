{ _1 = ./some/import.dhall as Location
, _2 = ../data/simpleLocation.dhall
, _3 = /absolute/import sha256:f9340badf94a684e652e0a384f64363293d8b632d971f3453f7ee22f10ab6e75 as Location
, _4 = https://prelude.dhall-lang.org/package.dhall as Location
, _5 = env:HOME as Location
, _6 = missing as Location
, _7 = (missing as Location) ? 42  -- `missing` fails as an import, but definitely resolves as Location
}
