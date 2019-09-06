let T1 = { a : Natural }
let T2 = { a : Natural, b : Text }
let T3 = { z : List { a : Natural, b : Text } }
let T4 = { z : List { b : Text, c : List Natural } }

let Prelude/List/map = ../../../../Prelude/List/map

let f = λ(old : T1) → old ⫽ { b = "Hello" }
let g = λ(old : T2) → { z = [ old, old.{ a } ⫽ { b = "Bye" } ] : List T2 }
let h_ = λ(old : T2) → old.{ b } ⫽ { c = [ old.a ] : List Natural }
let h =
        λ(old : T3)
      → { z = Prelude/List/map T2 { b : Text, c : List Natural } h_ old.z }

let k = λ(old : T1) → h (g (f old))

in k
