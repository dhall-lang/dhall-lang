  { render =
        ./render.dhall
          sha256:36befdd8bb5a1c2b372709da245a8d074533b86429e137b894c08ad16fa34836
      ? ./render.dhall
  , renderCompact =
        ./renderCompact.dhall
          sha256:e6c8809fbe193fddd430f94350d69cefd45e7aaf8bd379e51b750fde75008562
      ? ./renderCompact.dhall
  , renderYAML =
        ./renderYAML.dhall
          sha256:bc71449397bbf48103c3ebbdd570cd27313115e94b2b1b96761d257d5c02d478
      ? ./renderYAML.dhall
  , omitNullFields =
        ./omitNullFields.dhall
          sha256:e6850e70094540b75edeb46f4d6038324a62def8d63544a1e9541f79739db6f0
      ? ./omitNullFields.dhall
  , tagInline =
        ./tagInline.dhall
          sha256:49559ac11906ba6cc9eac25753e31e7addb13bc760df108024174c55523984c4
      ? ./tagInline.dhall
  , tagNested =
        ./tagNested.dhall
          sha256:93a7415853b7677c832246efadc8e880c1b641a23589286a836a384ca311d26f
      ? ./tagNested.dhall
  }
∧ (   ./core.dhall
        sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
    ? ./core.dhall
  )
