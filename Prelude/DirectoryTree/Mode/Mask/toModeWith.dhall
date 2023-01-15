{-| @toModeWith x m@ converts some fields of a `Mask` @m@ to a `Mode`: If the
respective field is set in the first `Mask` @x@, then the flag will be set to
the flags value given in @m@. Otherwise, the value set in @m@ is ignored and the
value of the flag in the result is `None`.

The function is motivated by the following use case:
Alice has her umask set to 022 and new files will have permissions set to
rw-r--r--. Bob has the umask set to 077, hence newly created files will have
rw------- permissions. Now Eve distributes Dhall code writing a file with the
following mode to the filesystem:
```dhall
let mask = ./other.dhall

let permissions = ./all.dhall

in
toModeWith mask permissions
```
The written file will have rw-r--rwx permissions for Alice and rw----rwx
permissions for Bob as the `toModeWith mask` produces a `Mode` that will only
apply the permissions for the 'other' part.
-}
let Access =
        ../../Access/Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../../Access/Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38

let Access/Mask =
        ../../Access/Mask/Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ../../Access/Mask/Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df

let Mode =
        ../Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ../Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60

let Mask =
        ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471

let Access/equal =
        ../../Access/equal.dhall
          sha256:5fa90f55505780a7be942275d6bbb2b1f1fb7857364332ed732a0241c2165e53
      ? ../../Access/equal.dhall
          sha256:5fa90f55505780a7be942275d6bbb2b1f1fb7857364332ed732a0241c2165e53

let Access/toAccessWith =
        ../../Access/Mask/toAccessWith.dhall
          sha256:814ab74ad292c121620a1f468837d4a5473323423bf68c1bceca69e7b3c59077
      ? ../../Access/Mask/toAccessWith.dhall
          sha256:814ab74ad292c121620a1f468837d4a5473323423bf68c1bceca69e7b3c59077

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
                  sha256:7cac21e2b72cadf3ee0bf10680df4902ca73b6ee070219df5eac1a24cd66ccdf

        let Mode/none =
                ../none.dhall
                  sha256:0ed46da7e6acbdff9e4c9e27a9f2770075a7cd6cb6bb565765c62093df1b5563
              ? ../none.dhall
                  sha256:0ed46da7e6acbdff9e4c9e27a9f2770075a7cd6cb6bb565765c62093df1b5563

        in  assert : toModeWith none a === Mode/none

let example1 =
      \(a : Mask) ->
        let toMode =
              ./toMode.dhall
                sha256:af2b0dab799374afa0a2f28551446760ff29f4697c200da0a8b0a8def7feee2a

        let all =
              ./all.dhall
                sha256:758415eca8dfee675dfef93ace9af82abb36bb3319b8e6295537ed18f9b5d3dd

        in  assert : toModeWith all a === toMode a

let example2 =
      let mask =
            ./other.dhall
              sha256:94bf82678d8d1c4f370a96f3831d3ad4464fbee508ffb37e93a479a1d9ee25cf

      let permissions =
            ./all.dhall
              sha256:758415eca8dfee675dfef93ace9af82abb36bb3319b8e6295537ed18f9b5d3dd

      let rwx = { read = Some True, write = Some True, execute = Some True }

      in    assert
          :     toModeWith mask permissions
            ===  { user = None Access, group = None Access, other = Some rwx }

let example3 =
      let union =
            ./union.dhall
              sha256:3bb32fc6ea21f661571e46d90e450e0340b2f2b65482ba7bd1a30514e1b39fc2

      let group =
            ./group.dhall
              sha256:faa567630372f77bb5cd2fa4fe7cb4760d2f9f79e35df80917e6e0064decf7cf

      let other =
            ./other.dhall
              sha256:94bf82678d8d1c4f370a96f3831d3ad4464fbee508ffb37e93a479a1d9ee25cf

      let permissions =
            ./all.dhall
              sha256:758415eca8dfee675dfef93ace9af82abb36bb3319b8e6295537ed18f9b5d3dd

      let mask = union group other

      let rwx = { read = Some True, write = Some True, execute = Some True }

      in    assert
          :     toModeWith mask permissions
            ===  { user = None Access, group = Some rwx, other = Some rwx }

in  toModeWith
