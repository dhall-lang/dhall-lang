let Foo = < Bar : Natural | Baz : Natural >

in    λ(a : Type)
	→ λ(f : Natural → a)
	→ λ(ts : Foo)
	→ merge { Bar = λ(a : Natural) → f a, Baz = f } ts
