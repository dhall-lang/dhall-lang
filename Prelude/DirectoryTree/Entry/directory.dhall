let Entry =
        missing
          sha256:742610b2a13e55ae6e344b54aa8a7ee1bfec8e8b313a1132eae9286309b520e6
      ? ./entry.dhall

in  \(tree : Type) -> Entry (List tree)
