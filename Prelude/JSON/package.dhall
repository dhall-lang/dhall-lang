  { render =
        ./render sha256:f7c372fcc954bfbbc7f83deec2006608a48efa2b08e8753bfdf73dc0aa7b4faf
      ? ./render
  , renderYAML =
        ./renderYAML sha256:0949b834275fb231023ea80831d830352e0b89a15f4081f043cb83e745fdb6e8
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
