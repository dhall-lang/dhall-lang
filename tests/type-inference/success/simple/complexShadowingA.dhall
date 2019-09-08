let x = Bool
let x = Natural
in  \(x: x@1 {- Bool -})
 -> \(x: x@1 {- Natural -})
 -> let x = if x@1 then 0 else 1
    in  (x + x@1) : x@3
