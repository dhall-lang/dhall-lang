../../../../../../Prelude/List/build
Text
( λ(list : Type)
→ λ(cons : Text → list → list)
→ λ(nil : list)
→ cons "ABC" (cons "DEF" nil)
)
