  { render =
        ./render sha256:eb26e0a4eee530dcda9d73974e9e9256fbad264916101866406a1647e28e280d
      ? ./render
  , renderYAML =
        ./renderYAML sha256:d406a8744b81ced6a485da3cdb40a357fdd37338f4f134b00e7da3684054ebd3
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
