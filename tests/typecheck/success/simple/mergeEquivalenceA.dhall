    let Foo = < Bar : {} | Baz : {} >

in    λ(a : Type)
    → λ(f : {} → a)
    → λ(ts : Foo)
    → merge { Bar = λ(b : {}) → f b, Baz = f } ts
