# Release checklist

- [ ] compute the new version number by following the [versioning scheme][versioning-scheme]
- [ ] update `README.md` with the new version number ([here][readme-version-number]).
- [ ] update `CHANGELOG.md` with all the changes since the last version.  
      **N.B.**: this includes also unreleased changes in the `Prelude`.  
      The changes in each version should be classified in three sections
      (Note: the subdivision reflects the [versioning scheme numbers][versioning-scheme]):
  - `Breaking changes`
  - `New features`
  - `Other changes`
     
[readme-version-number]: https://github.com/dhall-lang/dhall-lang#development-status
[versioning-scheme]: https://github.com/dhall-lang/dhall-lang/blob/master/VERSIONING.md#versioning-scheme
