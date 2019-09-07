    let bottom : Type = ∀(any : Type) → any

in  let not : Type → Type = λ(p : Type) → p → bottom

in  let pow = λ(X : Kind) → X → Type

in  let U = ∀(X : Kind) → (pow (pow X) → X) → pow (pow X)

in  let tau
        : pow (pow U) → U
        =   λ(t : pow (pow U))
          → λ(X : Kind)
          → λ(f : pow (pow X) → X)
          → λ(p : pow X)
          → t (λ(x : U) → p (f (x X f)))

in  let sigma : U → pow (pow U) = λ(s : U) → s U (λ(t : pow (pow U)) → tau t)

in  let Delta
        : pow U
        = λ(y : U) → not (∀(p : pow U) → sigma y p → p (tau (sigma y)))

in  let Omega : U = tau (λ(p : pow U) → ∀(x : U) → sigma x p → p x)

in  let Theta : Type = ∀(p : pow U) → (∀(x : U) → sigma x p → p x) → p Omega

in  let D : Type = ∀(p : pow U) → sigma Omega p → p (tau (sigma Omega))

in  let lem1
        : ∀(p : pow U) → (∀(x : U) → sigma x p → p x) → p Omega
        =   λ(p : pow U)
          → λ(t1 : ∀(x : U) → sigma x p → p x)
          → t1 Omega (λ(x : U) → t1 (tau (sigma x)))

in  let lem3 : D = λ(p : pow U) → lem1 (λ(y : U) → p (tau (sigma y)))

in  let lem2
        : not D
        = lem1
          Delta
          (   λ(x : U)
            → λ(H2 : sigma x Delta)
            → λ(H3 : ∀(p : pow U) → sigma x p → p (tau (sigma x)))
            → H3 Delta H2 (λ(pp : pow U) → H3 (λ(y : U) → pp (tau (sigma y))))
          )

in  let evidence : bottom = lem2 lem3

in  evidence
