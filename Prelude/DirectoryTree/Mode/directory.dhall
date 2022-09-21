let rwx = ./../Access/rwx.dhall

let rx = ./../Access/rx.dhall

in  { user = Some rwx, group = Some rx, other = Some rx } : ./../Mode.dhall
