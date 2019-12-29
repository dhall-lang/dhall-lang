  { render =
        ./render sha256:81f6a6bc27c1f2a6207d778ae82fef4c7c2b4c7a1535bdfb789f7bdad70c54b1
      ? ./render
  , renderYAML =
        ./renderYAML sha256:ea3dd07f32918d9e21f2b0d3079b3e2e705f2593adb36710e00175996a565b95
      ? ./renderYAML
  , omitNullFields =
        ./omitNullFields sha256:eda1c75ce16a5b258224f3e01671973e20a34b171a9a869eab6f3d346b9aa3a9
      ? ./omitNullFields
  , tagInline =
        ./tagInline sha256:49559ac11906ba6cc9eac25753e31e7addb13bc760df108024174c55523984c4
      ? ./tagInline
  , tagNested =
        ./tagNested sha256:93a7415853b7677c832246efadc8e880c1b641a23589286a836a384ca311d26f
      ? ./tagNested
  }
∧ (   ./core.dhall sha256:0a73b23ced8d1a472aa77ec328bae9696b8246352e5424bb22f701fc6ec6eab6
    ? ./core.dhall
  )
