{- | @makeTreeFromList tree make parents xs@ places the trees @xs@ below the
directory @parent@. @parent@ is given as a non-empty list of 'Metadata' values
where each of those will be used to create the respective level of the directory
tree.
-}
let Make =
        ./Make.dhall
          sha256:235d511ed943dded33b46b1717df263037329394e27fb4b9c677eda5af924458
      ? ./Make.dhall

let Metadata =
        ../Entry/Metadata.dhall
          sha256:8c240a00094238a73904af63ac0924b3e6aba1655312f20a2a27f88554e2febe
      ? ../Entry/Metadata.dhall

let NonEmpty =
        ../../NonEmpty/Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ../../NonEmpty/Type.dhall

let makeTreesFromList =
        ./makeTreesFromList.dhall
          sha256:34ed54a0fc4072cbcb32d0b40adc3ad4e5d978c8dcd7fece9670bf0616f4ab18
      ? ./makeTreesFromList.dhall

let makeTreeFromList
    : forall (tree : Type) ->
      Make tree ->
      NonEmpty Metadata ->
      List tree ->
        tree
    = \(tree : Type) ->
      \(make : Make tree) ->
      \(parents : NonEmpty Metadata) ->
      \(leaves : List tree) ->
        make.directory
          (     parents.head
            /\  { content = makeTreesFromList tree make parents.tail leaves }
          )

in  makeTreeFromList
