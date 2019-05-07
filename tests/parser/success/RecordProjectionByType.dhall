let example1 = λ(A : Type) → λ(B : Type) → λ(C : { x : A, y : B }) → C.({ x : A }) : { x : A }

let example2 =
	  λ(A : Type)
	→ λ(B : Type)
	→ λ(C : { p : A, q : B })
	→ C.(let r = { p : A } in r) : { p : A }

let A = Natural

let B = Text

in

(example1 A B { x = 10, y = "Text" }) ∧ (example2 A B { p = 10, q = "Text" }) : { x : A, p : A }
