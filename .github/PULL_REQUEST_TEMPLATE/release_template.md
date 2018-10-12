# Release checklist

- [ ] compute the new version number by following the [versioning scheme][versioning]
- [ ] update `binary.md` with the new version number ([here][versioning]).
- [ ] update `CHANGELOG.md` with all the changes since the last version.  
      **N.B.**: this includes also unreleased changes in the `Prelude`.  
      The changes in each version should be classified in three sections
      (Note: the subdivision reflects the [versioning scheme numbers][versioning-scheme]):
  - `Breaking changes`
  - `New features`
  - `Other changes`
     
[versioning]: https://github.com/dhall-lang/dhall-lang/blob/master/standard/versioning.md
