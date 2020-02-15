# Deprecation of quoted paths in URLs

> Migrate your code to remove URLs with quoted path components

On July 1, 2019 the [language standard changed](https://github.com/dhall-lang/dhall-lang/pull/604) to allow RFC3986-compliant URLs.  This section describes what changed and how to migrate your code.

## Changes

Before this change, if you wanted to use special characters in a URL path component, you had to quote it:

```dhall
http://example.com/"foo<bar.dhall"
```

After this change, the correct way to format this URL is via percent-encoding:

```dhall
http://example.com/foo%3Cbar.dhall
```

All RFC 3986 style URLs are permitted, except that we do not permit the characters `(`, `)` or `,` in URLs to prevent confusion with surrounding Dhall code.

## Phases

The old URL quoted-path syntax will be phased out in three steps:

* Phase 1 - Allow standard RFC 3986 URLs

  * Standard version: [9.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v9.0.0)
  * `dhall` version: [1.25.0](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.25.0)

  The first phase is backwards compatible.  The only difference is that RFC 3986 standard URLs are now permitted, including percent-encoding.

* Phase 2 - `dhall format` now rewrites quoted URLs as percent-encoded URLs

  * `dhall` version: [1.26](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.26.0)
  
  This change does not change language behaviour; but it means that code using deprecated syntax will be automatically formatted to use the new syntax.

* Phase 3 - quoted URLs are no longer allowed.

  This change is backwards incompatible by removing support for the old quoted-path-component syntax for URLs, breaking all code that still uses the old syntax.

  * Standard version: [17.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v17.0.0)

## Migration

* Phase 1 - No action required

* Phase 2 - Automatically migrate your code

   During this phase `dhall format` will automatically migrate the old quoted-path URL syntax to use percent-encoding instead to ease the migration process.  This is a safe and behavior-preserving transformation.

* Phase 3 - Your code breaks if you haven't migrated

  During Phase 3 the deprecation cycle is complete and if you haven't migrated then your code will fail to parse.
