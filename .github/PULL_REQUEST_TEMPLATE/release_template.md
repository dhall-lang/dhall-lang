# Release checklist

Update `CHANGELOG.md` with all the changes since the last version

**NOTE**: this includes also changes to the `Prelude`

The changes in each version should be classified in three sections

* `Breaking changes`

* `New features`

* `Other changes`

These three sections correspond to the components of the
[versioning scheme numbers][versioning-scheme])

## Updating the version number

The version number should up-to-date at all times, meaning that for all changes
to the standard:
     
*   Compute whether the version number should be changed by following the
    [versioning scheme][versioning]

*   If the version number should be changed:

    * Update [`versioning.md`][versioning] with the new version number

    * Update all semantic integrity checks within the test suite to reflect the
      new version number

      This is necessary since the version number is an input to the semantic
      integrity check.

      Since proposed changes to the standard should include a matching change to
      at least one implementation, you can use the matching implementation to
      compute the updated semantic integrity check.

Then no update to the version number should be necessary once you are ready to
cut a release.  Or, in other words, the version number should always be
"release-ready".

[versioning]: https://github.com/dhall-lang/dhall-lang/blob/master/standard/versioning.md
