let example1 = λ(A : Type) → λ(B : Type) → λ(C : { x : A, y : B }) → C.({ x : A }) : { x : A }

let example2 =
	  λ(A : Type)
	→ λ(B : Type)
	→ λ(C : { p : A, q : B })
	→ C.(let r = { p : A } in r) : { p : A }
in

example1 ⩓ example2 : { x : A, p : A }
