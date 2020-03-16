let T = Natural let ap = λ(f : T → List T) -> λ(x : T) -> f x in ap (λ(x : T) -> ap (λ(y : T) -> [x, y]) 1) 0
