let rwx = ./../AccessMask/rwx.dhall

let rx = ./../AccessMask/rx.dhall

in  { user = rwx, group = rx, other = rx } : ./Mask.dhall
