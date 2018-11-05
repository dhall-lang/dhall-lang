# Dhall Acceptance Tests

This folder contains the "acceptance tests suite" for the Dhall language.  
This means that while the tests are not part of the standard per se, a
standard-compliant
implementation is expected to comply with all the tests here.  
If this is not the case, then one of the following holds:

1. the implementation is not standard-compliant
2. the implementation is standard-compliant but the standard is underspecified
   and the other implementations are relying on some assumption. This should be
   considered a bug and the assumption in question should be added to the
   standard
3. the implementation is standard-compliant and there's a bug in the tests. This
   should be considered a bug for the test-suite

Please open an issue if you stumble on either `2` or `3`.

## How to run these tests

The tests are split by the feature that they are testing (we generally aim to
test only one feature at a time) into different folders: `normalization`,
`typecheck`, etc.  
Please see the next sections on more details on what to run exactly for every
test suite.

Inside each of them, the tests are split by the expected result:
- `success`: there is always two files for each test case:
  - `${TestcaseName}A.dhall` is the expression to evaluate
  - `${TestcaseName}B.dhall` is the result that the interpreter is supposed to
    return
- `failure`: the interpreter is expected to error out when evaluating each test
  case
- other folders at the same depth level - like `data` - can be ignored, as they
  only contain data to support the other tests).

At the next depth level (that is, inside `success`, etc) tests are grouped by
some other common feature they might have. This split is not functionally
relevant (unlike the previous two), but just to keep things tight.  
Some common subdivisions at this level:
- `prelude`: test for the Prelude
- `simple`: tests that only exercise the feature in question (e.g.
  `normalization` contains tests which require the import system to work
  correctly, except for the ones categorized as `simple`, which don't depend on
  the import feature implementation)

But how should every feature be exercised? The following sections detail what to
run for each feature:

### Running `normalization` tests

The tests should:
- parse `A` and `B`
- normalize `A` and `B`
- the results should match

Where `A` and `B` are:
- `A`: unnormalized text
- `B`: normalized text

### Running `typechecking` tests

The tests should:
- parse `A` and `B`
- build an Annotation from `A` and `B` such that `A : B`
- the Annotation should typecheck

Where `A` and `B` are:
- `A`: normalized text
- `B`: type of `A`

(Note: for the `failure` tests we don't build an Annotation, but just typecheck)

### Running `import` tests

The tests should:
- parse `A` and `B`
- resolve the imports for both `A` and `B`
- the results should match

Where `A` and `B` are:
- `A`: normalized text
- `B`: type of `A`

