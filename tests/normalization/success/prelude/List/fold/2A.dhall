  λ(list : Type)
→ λ(cons : Natural → list → list)
→ λ(nil : list)
→ (../../../../../Prelude/package.dhall).`List`.fold Natural [ 2, 3, 5 ] list cons nil
