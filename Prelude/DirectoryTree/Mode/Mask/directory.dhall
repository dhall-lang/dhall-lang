let rwx = ../../Access/Mask/rwx.dhall

let rx = ../../Access/Mask/rx.dhall

in  { user = rwx, group = rx, other = rx } : ./Type.dhall
