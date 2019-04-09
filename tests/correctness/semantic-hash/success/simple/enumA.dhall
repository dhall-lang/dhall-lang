let Role = < Wizard | Fighter | Rogue >

let show : Role → Text
         =
        λ(x : Role)
      → merge { Wizard = "Wizard", Fighter = "Fighter", Rogue = "Rogue" } x

in  show Role.Wizard
