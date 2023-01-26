let Metadata =
        ./Metadata.dhall
          sha256:8c240a00094238a73904af63ac0924b3e6aba1655312f20a2a27f88554e2febe
      ? ./Metadata.dhall

in  \(content : Type) -> Metadata //\\ { content : content }
