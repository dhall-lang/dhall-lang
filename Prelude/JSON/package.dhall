  { render =
        ./render sha256:a5229482f11d5b6e499ffb184aed55410463a3f197b847b7bca0834160e72cbe
      ? ./render
  , renderYAML =
        ./renderYAML sha256:dad92a120956ae852be7496d1e1955bbf3f60834ab2ad32641269098f2846477
      ? ./renderYAML
  , omitNullFields =
        ./omitNullFields sha256:e6850e70094540b75edeb46f4d6038324a62def8d63544a1e9541f79739db6f0
      ? ./omitNullFields
  , tagInline =
        ./tagInline sha256:49559ac11906ba6cc9eac25753e31e7addb13bc760df108024174c55523984c4
      ? ./tagInline
  , tagNested =
        ./tagNested sha256:93a7415853b7677c832246efadc8e880c1b641a23589286a836a384ca311d26f
      ? ./tagNested
  }
∧ (   ./core.dhall sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
    ? ./core.dhall
  )
