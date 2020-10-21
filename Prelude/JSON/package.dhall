  { render =
        ./render.dhall sha256:b5e5aee66b8fc018b50e1019a76340deafe5f8176854ea3611476d79990785da
      ? ./render.dhall
  , renderCompact =
        ./renderCompact.dhall sha256:e6c8809fbe193fddd430f94350d69cefd45e7aaf8bd379e51b750fde75008562
      ? ./renderCompact.dhall
  , renderYAML =
        ./renderYAML.dhall sha256:dad92a120956ae852be7496d1e1955bbf3f60834ab2ad32641269098f2846477
      ? ./renderYAML.dhall
  , omitNullFields =
        ./omitNullFields.dhall sha256:e6850e70094540b75edeb46f4d6038324a62def8d63544a1e9541f79739db6f0
      ? ./omitNullFields.dhall
  , tagInline =
        ./tagInline.dhall sha256:49559ac11906ba6cc9eac25753e31e7addb13bc760df108024174c55523984c4
      ? ./tagInline.dhall
  , tagNested =
        ./tagNested.dhall sha256:93a7415853b7677c832246efadc8e880c1b641a23589286a836a384ca311d26f
      ? ./tagNested.dhall
  }
∧ (   ./core.dhall sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
    ? ./core.dhall
  )
