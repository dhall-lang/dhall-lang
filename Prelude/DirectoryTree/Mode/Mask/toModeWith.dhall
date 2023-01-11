{- | @toModeWith x m@ converts some fields of a `Mask` @m@ to a `Mode`: If the
respective field is set in the first `Mask` @x@, then the flag will be set to
the flags value given in @m@. Otherwise, the value set in @m@ is ignored and the
value of the flag in the result is `None`.
-}
let Access =
        ../../Access/Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../../Access/Type.dhall

let Access/Mask =
        ../../Access/Mask/Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ../../Access/Mask/Type.dhall

let Mode =
        ../Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ../Type.dhall

let Mask =
        ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall

let Access/equal =
        ../../Access/equal.dhall
          sha256:5fa90f55505780a7be942275d6bbb2b1f1fb7857364332ed732a0241c2165e53
      ? ../../Access/equal.dhall

let Access/toAccessWith =
        ../../Access/Mask/toAccessWith.dhall
          sha256:814ab74ad292c121620a1f468837d4a5473323423bf68c1bceca69e7b3c59077
      ? ../../Access/Mask/toAccessWith.dhall

let f
    : Access/Mask -> Access/Mask -> Optional Access
    = \(set : Access/Mask) ->
      \(m : Access/Mask) ->
        let x = Access/toAccessWith set m

        in  if    Access/equal
                    { execute = None Bool, read = None Bool, write = None Bool }
                    x
            then  None Access
            else  Some x

let toModeWith
    : Mask -> Mask -> Mode
    = \(set : Mask) ->
      \(m : Mask) ->
        { user = f set.user m.user
        , group = f set.group m.group
        , other = f set.other m.other
        }

let example0 =
      \(a : Mask) ->
        let none =
                ./none.dhall
                  sha256:7cac21e2b72cadf3ee0bf10680df4902ca73b6ee070219df5eac1a24cd66ccdf
              ? ./none.dhall

        let Mode/none =
                ../none.dhall
                  sha256:0ed46da7e6acbdff9e4c9e27a9f2770075a7cd6cb6bb565765c62093df1b5563
              ? ../none.dhall

        in  assert : toModeWith none a === Mode/none

in  toModeWith
