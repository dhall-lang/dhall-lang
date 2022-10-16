let r = ../../Access/Mask/r.dhall

let rw = ../../Access/Mask/rw.dhall

in  { user = rw, group = r, other = r } : ./Type.dhall
