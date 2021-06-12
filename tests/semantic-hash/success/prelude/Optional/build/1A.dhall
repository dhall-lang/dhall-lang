../../../../../../Prelude/Optional/build.dhall
Natural
( λ(optional : Type)
→ λ(some : Natural → optional)
→ λ(none : optional)
→ none
)
