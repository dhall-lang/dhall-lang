# Dhall Acceptance Tests

This folder contains the "acceptance tests suite" for the Dhall language.  
This means that while the tests are not part of the standard per se, a
standard-compliant implementation is expected to successfully manage through all
the tests here.  
If this is not the case, then one of the following holds:

1. the implementation is not standard-compliant
2. the implementation is standard-compliant but the standard is underspecified
   and the other implementations are relying on some assumption. This should be
   considered a bug of the specification and the assumption in question should
   be added to the standard
3. the implementation is standard-compliant and there's a bug in the tests. This
   should be considered a bug for the test suite

Please open an issue if you stumble on either 2. or 3.

## How to run these tests

The tests are split by the feature that they are testing (we generally aim to
test only one feature at a time) into different folders: `normalization`,
`typecheck`, etc.  
(Please see the next sections on more details on what to do for every test
suite)

Inside each of them, the tests are split by the expected result:
- `success`: there is always two files for each test case:
  - `${TestcaseName}A.dhall` is the expression to evaluate
  - `${TestcaseName}B.dhall` is the result that the interpreter is supposed to
    return
- `failure`: the interpreter is expected to error out when evaluating each test
  case
- other folders at the same depth level - like `data` - can be ignored, as they
  only contain data to support the other tests.

At the next depth level (that is, inside `success`, etc) tests are grouped by
some other common feature they might have. This split is not functionally
relevant (unlike the previous two), but just to keep things tight.  
Some common subdivisions at this level:
- `prelude`: test for the Prelude
- `simple`: tests that only exercise the feature in question (e.g.
  `normalization` contains some tests which require the import system to work
  correctly, except for the ones categorized as `simple`, which don't depend on
  the import system being implemented)

But how should every feature be exercised? The following sections detail what to
run for each feature:

### Running `parser` tests

The tests should:
- parse `A` and encode it to its binary CBOR representation
- read `B` as bytes
- the results should match

Where `A` and `B` are:
- `A`: a Dhall expression as text
- `B`: the binary representation of the Dhall-CBOR-encoded expression

*Note*: since the tests are binary-encoded, they are not the friendliest to debug.  
For this reason, you might want to look at the `diff-binary.sh` script to help
with the debugging.

### Running `normalization` tests

The tests should:
- parse `A` and `B`
- eventually resolve the imports in both (if not running `simple` or `unit` tests)
- normalize them both
- the results should match

Where `A` and `B` are:
- `A`: unnormalized text
- `B`: normalized text

### Running `α-normalization` tests

The tests should:
- parse `A` and `B`
- α-normalize them both
- the results should match

Where `A` and `B` are:
- `A`: unnormalized text
- `B`: normalized text

### Running `typecheck` tests

The tests should:
- parse `A` and `B`
- eventually resolve the imports in both (if not running `simple` or `unit` tests)
- build an Annotation such that `A : B`
- the Annotation should typecheck

Where `A` and `B` are:
- `A`: normalized text
- `B`: type of `A`

(Note: for the `failure` tests we don't build an Annotation, but just typecheck)

### Running `type-inference` tests

The tests should:
- parse `A` and `B`
- eventually resolve the imports in both (if not running `simple` or `unit` tests)
- infer the type of A
- the inferred type of A should exactly match B

### Running `import` tests

The tests should:
- parse `A` and `B`
- resolve the imports for both `A` and `B`
- the results should match

Where `A` and `B` are:
- `A`: text with unresolved imports
- `B`: text where all the imports have been resolved, normalized and replaced with their value

