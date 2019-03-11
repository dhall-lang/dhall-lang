../../../../../../Prelude/Natural/build
( λ(natural : Type)
→ λ(succ : natural → natural)
→ λ(zero : natural)
→ succ (succ (succ zero))
)
