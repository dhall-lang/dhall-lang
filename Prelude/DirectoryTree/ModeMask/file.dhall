let r = ./../AccessMask/r.dhall

let rw = ./../AccessMask/rw.dhall

in  { user = rw, group = r, other = r } : ./Mask.dhall
