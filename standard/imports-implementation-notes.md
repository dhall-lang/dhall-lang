# Implementation notes on Dhall import system

This document is a pedagogical companion to `imports.md` that explains step
by step the import mechanism specified by the Dhall standard.

## Overview of imports and import resolution

### Kinds of imports

An import is a Dhall expression that references an external resource that needs
to be read. The external resource may be a local file, an environment variable,
or an Internet URL. For example:

```dhall
let user = env:USER as Text        -- get the local username from the shell; the import expression is `env:USER as Text`
let privateEnv = ./env.dhall       -- read a local file by relative path; the import expression is `./env.dhall`
let lessThan = https://prelude.dhall-lang.org/Natural/lessThan -- read from the Web; the import expression is the URL
...
```

"Code imports" are Dhall import expressions that interpret the contents of the external resource as Dhall code.
That code could again contain imports. Code imports either have an `as Source` qualifier or do not have an `as NNN` qualifier.

"Data imports" are Dhall import expressions that read the contents of the external resource as data rather than as Dhall code.
Data imports have a qualifier `as Text` or `as Bytes`.

"Location imports" have a qualifier `as Location` and do not read the contents of the external resource at all.

"Void imports" are represented by the keyword `missing`. A void import means a resource that is missing _by definition_; no attempt will be made to fetch anything.

### Import resolution

Import resolution is the process that takes a Dhall expression that may contain imports and
replaces those imports with expressions or data fetched from the external resources.

At a high level, "resolving" an import means to:

1. Determine which resource the import refers to.
2. Read the data from that resource or from the cache if available. (But see below about imports `as Location` that don't actually require reading any external resources.)
3. Parse the fetched text data as a Dhall expression (if it is a code import) or in another way.
4. For code imports: Recursively resolve any imports inside the fetched expression (unless the import mode does not require recursive resolution).
5. After recursively resolving all imports, the imported code typically needs to be typechecked.
6. Normalize the result (unless it is an import `as Source`).
7. Possibly compute the hash of the result and cache it on disk.
8. Substitute the final result instead of the import expression into the parent expression.

The main complications come from:

- relative imports, which must be interpreted relative to the importing resource path
- import cycles, which must be detected and rejected
- referential sanity checks and CORS checks for remote imports (see below)
- import alternatives (`e0 ? e1`) that require error handling
- integrity checks (`sha256:...`)
- standard-mandated caching behavior: repeated imports must give the same results
- the distinction between ordinary code imports and code imports `as Source`

## Supported import types and import modes

Every import has both:

- an import _type_, which says where to fetch from
- an import _mode_, which says how to interpret what was fetched

Optionally, an import can also have a SHA256 hash. Such imports are called "hash-protected" or "frozen". We will discuss this later.

The supported import types are:

- local file imports
- remote URL imports
- environment variable imports
- `missing`

The supported import modes are:

- importing `as Text`: the result is a Dhall `Text` value representing the resource data
- importing `as Bytes`: the result is a Dhall `Bytes` value representing the resource data
- importing `as Location`: the result is a Dhall `Location` value representing the resource location (no reading of the resource data required!)
- importing `as Source`: the result is a Dhall expression without full normalization
- default (no mode annotation): the result is a fully normalized Dhall expression

In the current Haskell implementation, these modes are represented by the constructors `RawText`, `RawBytes`, `Location`, `Code`, and `Source`.

The import mode matters a lot:

- `as Text`, `as Bytes`, and `as Location` do not need to run the code-import pipeline
- an ordinary code import ends up as a fully resolved, typechecked, normalized expression
- an `as Source` import does not normalize the imported source code (reducing the size of the normal form in some cases)

## The import resolution context

Import resolution is not just a recursive function on expressions: It also
threads a changing context through the recursion.

The Dhall standard denotes this context as:

- the import stack: a non-empty ordered list of already visited imports, written `(Δ, here)`
- a map from imports to expressions, written `Γ`, encompassing imports that have been already fully resolved and/or cached

An implementation needs at least the following information in the import resolution context:

- the current import stack
- the mapping `Γ` that represents already available or already cached imports
- configuration for remote fetching: custom headers
- configuration for integrity-check caches (including a flag indicating whether caching is enabled)

The current Haskell implementation uses a `Status` value as the import resolution context.
The `Status` value stores the information shown above and also some more information.

### The import stack

The import stack answers the question: From what chain of already-resolved
imports are we now resolving the current import?

That information is needed for:

- resolving relative imports
- detecting cycles
- building error messages
- applying referential sanity checks
- applying CORS checks


The import stack is never empty. Although the Dhall standard does not specify
what the initial value of the import stack must be, a Dhall implementation
needs to represent the initial base directory (the current directory of the
process) somehow in the first element of the import stack.

To help build the intuition, let us visualize the initial import stack like this:

```text
[ fake-root-for-current-directory ]
```

When we evaluate the Dhall expression `./program.dhall`, we begin by resolving
the import `./program.dhall` in the current directory. While we are reading
`./program.dhall`, the import stack looks like this:

```text
[ ./program.dhall, fake-root-for-current-directory ]
```

We proceed to resolve imports within the code of `program.dhall`.
If `program.dhall` imports `https://prelude.dhall-lang.org/Natural/lessThan` then
we need to resolve that, and the import stack becomes:

```text
[ https://prelude.dhall-lang.org/Natural/lessThan, ./program.dhall, fake-root-for-current-directory ]
```


### The two standard maps

The standard writes `Γ` as if it were one map. Pedagogically, it is useful to
separate three different roles that practical implementations may give to `Γ`:

1. The external world: file contents, URL contents, environment variables.
2. The semantic cache for hash-protected imports.
3. Reuse of already resolved imports within one run.

The current Haskell implementation has several concrete mechanisms:

- an in-memory cache of already resolved imports
- the semantic cache under `.cache/dhall/`
- a separate "semi-semantic" cache under `.cache/dhall-haskell/`

Only the semantic cache is mandated by the standard. The others are
implementation techniques.

### Parent imports, child imports, chaining, and canonicalization

The Dhall standard specifies that the stack is a non-empty ordered list of imports
currently being traversed. Each stack entry is a "canonicalized" and "chained" import reference.
We will discuss the operations of "canonicalization" and "chaining" in detail now.

Canonicalization means removal of any `../` and of any non-initial `./` from the file paths and from the URLs.

So, an import `./././a/b/../c.dhall` is canonicalized to `./a/c.dhall`.

Before talking about "chaining", it helps to define the "parent" and "child" imports.

- The **parent import** is the import whose contents are currently being read.
- A **child import** is an import expression found inside the parent's contents.

For example:

```dhall
-- This file is ./schema/hobbies.dhall
././hobby.dhall
```

Here, while we are resolving imports in the file `./schema/hobbies.dhall`:

- the parent import is `./schema/hobbies.dhall`
- the child import is `././hobby.dhall`

The Dhall standard specifies that relative-path imports (such as `././hobby.dhall`) are resolved
with respect to the parent import's directory or URL.
In this case, the import `././hobby.dhall` is to be interpreted relative to the parent import.
Therefore, the actual resource to fetch is `./schema/hobby.dhall`.

The operation of finding the actual resource to fetch is called **chaining**:

1. start from the parent import
2. combine it with the raw child import
3. canonicalize the result

For the example above:

- raw child import: `././hobby.dhall`
- parent import: `./schema/hobbies.dhall`
- chained child import: `./schema/./hobby.dhall`
- canonicalized child import: `./schema/hobby.dhall`

So a **non-chained import** is just the raw import syntax found in the source
text, while a **chained import** is the canonical import obtained after taking
the parent into account.

The standard talks about this with judgments such as:

```text
parent </> import0 = import1
canonicalize(import1) = child
```

where:

- `parent` is the already-known (and already canonicalized and chained) parent import
- `import0` is the raw child import as it appeared in the source
- `parent </> import0` is the chained child import
- `child` is the child import after chaining and canonicalization

### Example: how chaining works in the import stack

Suppose we are evaluating this Dhall expression:

```dhall
./program.dhall
```

Then the import stack is initially like this:

```text
[ fake-root-for-current-directory ]
```

Suppose `./program.dhall` imports `./lib/util/other.dhall`, then the stack becomes:

```text
[ ./lib/util/other.dhall, ./program.dhall, fake-root-for-current-directory ]
```

Now if `./lib/util/other.dhall` imports `../types.dhall`, that child import is
resolved relative to `./lib/util/other.dhall`, not relative to the process working
directory. The "chaining" of the raw child import `../types.dhall` against the parent
import `./lib/util/other.dhall` gives the actual path to the child import
relative to the process working directory: `./lib/util/../types.dhall`.
Canonicalizing that path gives `./lib/types.dhall`.
So, the import stack becomes:

```text
[ ./lib/types.dhall, ./lib/util/other.dhall, ./program.dhall, fake-root-for-current-directory ]
```

The "chaining" and "canonicalization" operations compute `./lib/types.dhall` from `./lib/util/other.dhall` and `../types.dhall`.

A similar process is applied to Internet URL imports that themselves contain relative-path imports.
For example, if `https://server.com/library1.dhall` imports `./util/other.dhall` then
the canonicalized and chained path to that import is `https://server.com/util/other.dhall`.



## Step 1: chain the child import against the parent

Suppose a file contains:

```dhall
./schema/hobbies.dhall
```

and `./schema/hobbies.dhall` contains:

```dhall
./hobby.dhall
```

The second import does not mean `./hobby.dhall` relative to the process working
directory. It means `./schema/hobby.dhall`, because it is first chained against
the parent import.

This first transformation is:

1. take the raw child import
2. chain it against the parent import
3. canonicalize the result

Canonicalization removes `.` and simplifies `..` where possible.

This is why the standard talks about canonical imports rather than raw source
text: the code uses the canonical child import, not the literal text
that appeared in the parent file.

## Step 2: reject invalid parent-child relationships

Before reading any resource, the resolver performs checks that depend only on
the parent and the canonical child import.

### Referential sanity

The standard's referential sanity rules are:

- a remote parent import may import:
  - another remote import
  - `missing`
  - `as Location`
- a remote parent import may not import:
  - a local file import
  - an environment-variable import
- a non-remote parent import may import anything

So this is allowed:

```dhall
https://example.com/A.dhall
```

importing:

```dhall
https://example.com/B.dhall
```

and this is also allowed:

```dhall
https://example.com/A.dhall
```

importing:

```dhall
missing
```

But this is not allowed:

```dhall
https://example.com/A.dhall
```

importing:

```dhall
./local-secret.dhall
```

or:

```dhall
env:SECRET
```

Note that this check is made after chaining and canonicalization. So, a parent import `https://example.com/A.dhall` may contain the import expression `./local-secret.dhall` but then the actual child import will be `https://example.com/local-secret.dhall` rather than a local file.

What about `as Location`?

`as Location` is special because it does not fetch the child resource. It only
returns a Dhall value describing the import's location. So, all parent imports may
import anything `as Location`.

### Cycle detection

If the canonical child import is already on the current stack, that is a cycle
and import resolution fails immediately.

For example:

```dhall
-- This file is foo.dhall
./bar.dhall
```

```dhall
-- This file is bar.dhall
./foo.dhall
```

When resolving `./foo.dhall`, we will start resolving `./bar.dhall` and the import stack will be:

```text
[ ./bar.dhall, ./foo.dhall, fake-root-for-current-directory ]
```

As we read `./bar.dhall`, we find that we have to resolve `./foo.dhall` again. We start doing that and find that the import stack would have to become this:


```text
[ ./foo.dhall, ./bar.dhall, ./foo.dhall, fake-root-for-current-directory ]
```

In this way, we detect an import cycle (which is always a fatal error).
The stack already contained the import `./foo.dhall` when we started resolving `./foo.dhall`.

## Step 3: fetch according to the import mode

When all the checks succeed, the resolver proceeds to fetch and process the imports depending on the mode.

### Ordinary code imports

For an ordinary code import:

1. fetch the text
2. parse the text as a Dhall expression
3. recursively resolve child imports inside it
4. replace each child import by the final result of recursively resolving that child import
5. typecheck the resulting Dhall expression
6. fully normalize the resulting Dhall expression

The resolver returns that normalized expression. This is the final result of import resolution for a given import expression.

### `as Source`

`as Source` behaves like an ordinary code import in the sense that it still:

- fetches text
- parses it as a Dhall expression
- recursively resolves child imports
- typechecks the full result

But unlike an ordinary code import, imported expressions are not normalized after every import and not always inlined.

Instead:

- non-hash-protected transitive imports are recursively resolved `as Source` (even if they are not marked as such) and are inlined into the parent Dhall expression _without full normalization_
- hash-protected transitive imports _remain unchanged_ in the parent Dhall expression

### `as Text` and `as Bytes`

For `as Text` and `as Bytes`, the resolver does not parse the fetched contents
as Dhall code.

Instead:

- `as Text` fetches raw text and returns the corresponding Dhall `Text` literal
- `as Bytes` fetches raw bytes and returns the corresponding Dhall `Bytes` literal

Note: the integrity check, if present, applies to the encoded Dhall literals, not to
the raw file contents by themselves. It is not the SHA256 of the raw contents; it's the SHA256 of the CBOR-encoded Dhall value.

### `as Location`

For `as Location`:

1. do not fetch the resource at all
2. chain and canonicalize the import
3. return a special Dhall value describing the location

This is why an import `as Location` does not even require the resource to exist and does not attempt to fetch any data from it.

## Caching behavior

The easiest way to understand ordinary imports is to separate three cases:

1. there is a reuse hit inside the current run
2. there is an on-disk semantic cache hit
3. the import must be fetched and resolved from scratch

### Case A: reuse within the current run

The standard requires that importing the same canonical path twice in one run
must return the same result.

A practical implementation can satisfy this by caching already resolved imports
in memory for the duration of one import-resolution run.

This is what the current Haskell implementation does. Its in-memory cache is
not written to disk.

### Case B: semantic cache hit

If the import has an integrity check:

```dhall
./foo.dhall sha256:...
```

then the resolver first tries to load the corresponding cached product from the
semantic cache, using the hash as the lookup key.

Typical semantic cache locations are:

- `${XDG_CACHE_HOME}/dhall/1220${base16Hash}`
- `${HOME}/.cache/dhall/1220${base16Hash}`
- `${LOCALAPPDATA}\dhall\1220${base16Hash}` on Windows

For an ordinary import, the semantic cache stores the encoded, fully resolved,
alpha-beta-normalized expression.

For an `as Source` import, the semantic cache stores the expression as it was in the source code of the imported resource, without normalization.

If the file is found in the cache and its SHA-256 matches the requested hash, the import
contents is read from the cache without refetching the original resource.

### Case C: resolve from scratch

If the import is not already available from the caches, then the resolver:

1. fetches the resource
2. parses the Dhall expression
3. recursively resolves transitive imports
4. typechecks the resulting import-free expression
5. beta-normalizes it
6. alpha-normalizes it for hashing when computing the semantic cache product
7. stores the resulting encoded expression in the semantic cache if an integrity
   check was requested

### Caching behavior with imports `as Source`

The current Haskell implementation is easiest to understand as a two-phase
process. The standard specifies the result, and this two-phase structure is one
reasonable way to realize it.

### Phase 1: build the source-preserving artifact

The resolver fetches and parses the import as Dhall code, then recursively walks
its transitive imports in a special mode.

That special mode asks:

> "If this child import is hash-protected, should I preserve the import node or
> inline it?"

That is why the current Haskell implementation uses two internal modes:

- preserve hashed imports
- inline hashed imports

Unhashed imports are always recursively resolved and inlined.

During this first phase:

- hashed transitive imports are validated, but preserved as import references
- unhashed transitive imports are recursively resolved and inlined

The result is a non-normalized expression that may still contain hash-protected
imports.

That expression is the artifact stored in the semantic cache for `as Source`.

### Phase 2: finalize for use by the parent

The cached `as Source` artifact is not the final runtime value.

Before returning a value to the caller, the resolver performs a second pass that
expands the remaining hashed imports.

Then it:

- checks that no imports remain
- applies substitutions if the implementation supports them
- typechecks the final import-free result

The result returned to the parent is import-free but did not go through
the full normalization procedure that an ordinary code import would.

### Example of using imports `as Source`

Suppose a large package imports many large definitions behind integrity checks.

With ordinary imports, every import must be reduced to the full normal form, and each subsequent import must inline all its transitive imports.

With `as Source`, the cached artifact can instead look like:

```dhall
{ A = missing sha256:...
, B = missing sha256:...
, C = ./small-unfrozen-file.dhall  -- fully inlined but not normalized
}
```

This preserves sharing and postpones expensive normalization work until the
final program evaluation.

## The three important caches

### 1. In-memory cache for one run

The standard requires same-result reuse within one run, but it does not require
any particular implementation technique.

The current Haskell implementation uses an in-memory cache keyed by the
canonical import value, including the import mode.

This cache is not written to disk.

Therefore:

```dhall
./foo.dhall
./foo.dhall as Source
```

are different keys for that cache.

So if a program imports the same resource once as ordinary code and once as
`as Source`, they are not treated as the same in-memory import entry.

### 2. Semantic cache for hash-protected imports

This is the standard content-addressed on-disk cache for imports protected by
SHA256 integrity checks.

Typical locations are:

- `${XDG_CACHE_HOME}/dhall/1220${base16Hash}`
- `${HOME}/.cache/dhall/1220${base16Hash}`
- `${LOCALAPPDATA}/dhall/1220${base16Hash}` on Windows

For ordinary imports, the semantic cache stores the encoded normalized
expressions.

For `as Source`, the semantic cache stores the encoded source-preserving
artifacts.

These use the same cache namespace and the same file naming convention.

However, ordinary imports and `as Source` imports usually produce different
semantic hashes, so they usually do **not** share the same semantic cache file.

They would share a semantic cache entry only in the special case where the two
import modes happen to produce exactly the same encoded CBOR bytes.

### 3. Semi-semantic cache for ordinary unhashed code imports

The current Haskell implementation also has a separate on-disk cache for
ordinary unhashed code imports.

Typical locations are:

- `${XDG_CACHE_HOME}/dhall-haskell/1220${base16Hash}`
- `${HOME}/.cache/dhall-haskell/1220${base16Hash}`

This cache is not mandated by the standard. It is an implementation
optimization.

More precisely:

- the cache key is computed from the fully resolved, not-yet-normalized import
  tree
- the cached value is the resulting normalized expression

This lets repeated ordinary code imports avoid redoing some of the expensive
work while still distinguishing imports whose fully resolved syntax differs
before normalization.

`as Source` does not use this semi-semantic cache path in the same way.

## Import alternatives: soft failures and hard failures

The import alternative operator:

```dhall
e0 ? e1
```

does **not** mean "if anything at all goes wrong, try `e1`".
It only recovers from "soft" failures.

The rule of thumb is:

- "resource is absent" is a soft failure
- "resource is present but bad" is a hard failure

This table explains it in detail:

| Failure mode | Soft or hard failure | Explanation |
|---|---|---|
| Missing file | Soft | The resource is absent. |
| Missing environment variable | Soft | The resource is absent. |
| Unreachable URL / retrieval failure | Soft | The resource is absent during resolution. |
| Explicit `missing` import | Soft | This is the canonical absent import. |
| Hash-protected import not present in semantic cache, and underlying resource absent | Soft | This is still an absence case. |
| Parse error in fetched Dhall code | Hard | The resource is present but its contents is malformed. |
| Type error in fetched Dhall code | Hard | The resource is present but the resulting code is invalid. |
| Hash-protected import is present in semantic cache but fails SHA256 integrity check | Hard | The resource is invalid, not matching the expected content. |
| Cyclic import | Hard | This is a structural error, not an absence of resource. |
| Referential sanity violation | Hard | This is a security policy violation, not an absence. |
| CORS violation | Hard | This is a security policy violation, not an absence. |


If a hash-protected import has a valid cache file then that file will be read, and Dhall will not try fetching the external resource. In that case it does not matter if the external resource cannot be read (file is not found, URL is not responding, environment variable is undefined).
The cached file takes precedence.

### Examples

This uses the fallback if the hashed value is absent from cache:

```dhall
missing sha256:... ? ./fallback.dhall
```

This does **not** use the fallback, because the import is present but fails the
integrity check:

```dhall
./foo.dhall sha256:wrong-hash ? ./fallback.dhall
```

### Import alternatives with imports `as Source`

For `as Source`, the branch that actually succeeds will determine what gets stored
in the cached artifact.

If the successful branch is hash-protected, the cached artifact keeps that branch as
a hash-protected import reference.

If the successful branch is not hash-protected, that branch is recursively resolved
and inlined.

So this:

```dhall
missing sha256:... ? ./Map/package.dhall
```

is **not** treated as "hash-protected as a whole" just because one branch has a
hash.

Its behavior depends on which branch actually resolves:

- if the hashed `missing sha256:...` branch is satisfied from cache, it remains
  as a hash-protected import reference
- if that branch is absent and the fallback is used, the fallback branch is
  unhashed and is recursively inlined

## Duplicate imports

The Dhall standard requires that importing the same canonical path twice in one run
must return the same result.

An implementation can satisfy this by caching, or by refetching and checking
that the result did not change.

If the import mode is part of the import identity in the implementation, then:

```dhall
./foo.dhall
./foo.dhall as Source
```

are not duplicate imports in the implementation sense, even though they refer to
the same underlying resource.

They request different import semantics and therefore usually produce different
results and different cache products.

## A worked example: simple relative imports

Suppose:

```dhall
-- ./program.dhall
./lib/func1.dhall
```

```dhall
-- ./lib/func1.dhall
./func2.dhall
```

```dhall
-- ./lib/func2.dhall
0
```

Resolution goes like this:

1. Start with stack `[ fake-root-for-current-directory ]`.
2. Resolve `./program.dhall` relative to `fake-root-for-current-directory`.
3. While reading `./program.dhall`, encounter `./lib/func1.dhall`.
4. Chain it against the parent and push it on the stack.
5. Read `./lib/func1.dhall`.
6. Encounter `./func2.dhall`.
7. Chain it against `./lib/func1.dhall`, producing `./lib/func2.dhall`.
8. Read `./lib/func2.dhall`.
9. Parse `0`.
10. No more imports remain, so return `0`.

The important point is step 7: the child import is interpreted relative to the
parent import, not relative to the process working directory.

## A worked example: `as Source`

Suppose:

```dhall
-- ./outer.dhall
./inner.dhall as Source
```

```dhall
-- ./inner.dhall
{ local = let x = 1 in x
, frozen = missing sha256:abc...
}
```

When building the `as Source` cached artifact:

- `local` is recursively resolved and inlined as syntax
- `frozen` is validated but preserved as an import reference

So the cached artifact is conceptually closer to:

```dhall
{ local = let x = 1 in x
, frozen = missing sha256:abc...
}
```

than to a fully normalized value.

Later, before returning the final result to the parent expression, the resolver
performs another pass that expands the remaining hashed imports too.

## Where to look in the Haskell implementation

The following files are useful if you want to see one concrete realization of
the standard:

- `dhall/src/Dhall/Import.hs`
- `dhall/src/Dhall/Import/Types.hs`
- `dhall/src/Dhall/Syntax/Import.hs`
- `dhall/src/Dhall/Import/Headers.hs`
- `dhall/src/Dhall/Freeze.hs`

The most relevant Haskell data structures are:

- `ImportType`
- `ImportMode`
- `Import`
- `ImportHashed`
- `Chained`
- `Status`

These names are useful when comparing the standard's abstract judgments to a
concrete implementation.

## Summary: a step-by-step import resolution algorithm

This is a practical step-by-step algorithm corresponding to the standard.

Given:

- an expression `expr`
- a non-empty stack of visited imports
- access to external resources and caches

do this:

1. Traverse `expr`.
2. If the current node is not an import, recursively resolve its subexpressions.
3. If the current node is an import:
   1. Let `parent` be the current stack head.
   2. Chain the raw child import against `parent`.
   3. Canonicalize the result to obtain the canonical child import.
   4. Check referential sanity and CORS compliance.
   5. Check whether the child import is already on the stack; if so, fail with
      a cycle error. Otherwise push the child import onto the stack.
   6. If the implementation reuses imports within one run, check that cache
      first.
   7. If the import is hash-protected, check the semantic
      cache using the hash as the lookup key.
   8. If there is a valid semantic cache hit, decode the cached product and skip to step 12 where the cached product is used.
   9. Otherwise, fetch the resource if the import mode requires fetching.
   10. Interpret the fetched resource according to the import mode:
       1. ordinary code import or `as Source`: parse as Dhall
       2. `as Text`: return a Dhall `Text` literal
       3. `as Bytes`: return a Dhall `Bytes` literal
       4. `as Location`: return a Dhall value describing the location
   11. Compute the cache product depending on the input mode:
       - For an ordinary code import: recursively resolve
         imports in the Dhall expression from the previous step, inlining each import into that expression.
         Then typecheck and fully normalize the resulting expression.
       - For an import `as Source`: recursively resolve imports in the Dhall
         expression from the previous step in the special mode where we do
         not normalize any resolved imports and do not inline hash-protected imports. 
   12. If the import is hash-protected, compute the SHA256 hash of the CBOR-encoded cache product.
       (For imports `as Source`, the cache product may be a non-normalized expression containing unresolved hash-protected imports.
       For all other imports, the cache product is a fully normalized expression without imports.)
       Check that the hash value agrees with what is given in sha256:... and if this does not
       match it's a hard failure of import. If the hash matches, save the encoded artifact to the cache on disk.
   13. If the mode is `as Source` then run another recursive resolving step where the remaining hash-protected child imports are inlined.
       This computes the final import-free result. (Note that the cache product may still contain unresolved hash-protected imports.)
   14. Pop the child import from the stack. Return the final Dhall result to the parent.
4. If the current node is an import alternative `e0 ? e1`:
   1. Try resolving `e0`.
   2. If `e0` fails because an import is absent and not already available from
      cache, resolve `e1` instead.
   3. If `e0` fails for any hard-failure reason, do not use the fallback.
5. Continue until no imports remain in the returned expression.
