let Entry =
        ./Type.dhall
          sha256:75148ae19175750e38705e11cda8dcc775b2ac08f22518ff2ef3f33a6273ef15
      ? ./Type.dhall

in  \(tree : Type) -> Entry (List tree)
