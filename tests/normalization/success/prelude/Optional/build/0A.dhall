(../../../../../../Prelude/package.dhall).`Optional`.build
Natural
( λ(optional : Type)
→ λ(some : Natural → optional)
→ λ(none : optional)
→ some 1
)
