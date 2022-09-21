let r = ./../Access/r.dhall

let rw = ./../Access/rw.dhall

in  { user = Some rw, group = Some r, other = Some r } : ./../Mode.dhall
