# Programmable configuration file

> Learn when it is appropriate to use the Dhall configuration language

```eval_rst
.. contents:: Table of Contents
   :backlinks: none

```

This section describes what a "programmable configuration file" is and the types
of problems that programmable configuration files solve.

## Repetition

Programmers commonly customize the behavior of their programs using
configuration files.  You might be familiar with configuration file formats such
as [JSON][JSON], [YAML][YAML], [XML][XML], [TOML][TOML] , and [INI][INI] files.
However, these configuration file formats are not programmable, so you cannot
simplify repetitive configuration blocks.

For example, consider the following JSON configuration file recording the
locations of public and private SSH keys for users:

```json
[
    {
        "privateKey": "/home/john/.ssh/id_rsa",
        "publicKey": "/home/john/.ssh/id_rsa.pub",
        "user": "john"
    },
    {
        "privateKey": "/home/jane/.ssh/id_rsa",
        "publicKey": "/home/jane/.ssh/id_rsa.pub",
        "user": "jane"
    },
    {
        "privateKey": "/etc/jenkins/jenkins_rsa",
        "publicKey": "/etc/jenkins/jenkins_rsa.pub",
        "user": "jenkins"
    },
    {
        "privateKey": "/home/chad/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "chad"
    }
]
```

This configuration file is mostly repetitive.  All users (except the `jenkins`
user) store their SSH keys in the same location relative to their home
directory.  We could simplify this configuration if every user saved their
public and private keys in a standard location relative to their home directory.
However, configuration files need to gracefully adapt to exceptional cases like
the `jenkins` user.

Humans can make mistakes when copying and pasting repetitive configuration
blocks.  For example, we might try to add a new user named `alice` by
duplicating the configuration for `chad` and replacing all occurrences of
`"chad"` with `"alice"`.

```json
[
    {
        "privateKey": "/home/john/.ssh/id_rsa",
        "publicKey": "/home/john/.ssh/id_rsa.pub",
        "user": "john"
    },
    {
        "privateKey": "/home/jane/.ssh/id_rsa",
        "publicKey": "/home/jane/.ssh/id_rsa.pub",
        "user": "jane"
    },
    {
        "privateKey": "/etc/jenkins/jenkins_rsa",
        "publicKey": "/etc/jenkins/jenkins_rsa.pub",
        "user": "jenkins"
    },
    {
        "privateKey": "/home/chad/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "chad"
    },
    {
        "privateKey": "/home/alice/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "alice"
    }
]
```

Oops!  We missed a spot and forgot to fix the path to `alice`'s public key.
This is a potential security breach if we install `chad`'s public key as an
authorized key on systems that only `alice` should have access to.

Repetitive configuration files are also harder to migrate as the configuration
format changes.  For example, suppose that we require a new field for every user
recording the path to their home directory:

```json
[
    {
        "home": "/home/john",
        "privateKey": "/home/john/.ssh/id_rsa",
        "publicKey": "/home/john/.ssh/id_rsa.pub",
        "user": "john"
    },
    {
        "home": "/home/jane",
        "privateKey": "/home/jane/.ssh/id_rsa",
        "publicKey": "/home/jane/.ssh/id_rsa.pub",
        "user": "jane"
    },
    {
        "home": "/home/jenkins",
        "privateKey": "/etc/jenkins/jenkins_rsa",
        "publicKey": "/etc/jenkins/jenkins_rsa.pub",
        "user": "jenkins"
    },
    {
        "home": "/home/chad",
        "privateKey": "/home/chad/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "chad"
    },
    {
        "home": "/home/alice",
        "privateKey": "/home/alice/.ssh/id_rsa",
        "publicKey": "/home/alice/.ssh/id_rsa.pub",
        "user": "alice"
    }
]
```

We could easily manage the migration for a small configuration file, but these
migrations increase in difficulty as the configuration file grows.

"Don't repeat yourself" (DRY) is a software engineering best practice, so why
not apply the same guideline to configuration files?

## Automation

We're better off programmatically generating the configuration file if we want
to avoid repeating ourselves.  One way to do this is to use the Dhall
configuration language, which you can think of as JSON + functions.

For example, we can rewrite our original JSON configuration as the following
Dhall configuration:

```dhall
-- config0.dhall

let ordinaryUser =
      \(user : Text) ->
        let privateKey = "/home/${user}/.ssh/id_rsa"

        let publicKey = "${privateKey}.pub"

        in  { privateKey, publicKey, user }

in  [ ordinaryUser "john"
    , ordinaryUser "jane"
    , { privateKey = "/etc/jenkins/jenkins_rsa"
      , publicKey = "/etc/jenkins/jenkins_rsa.pub"
      , user = "jenkins"
      }
    , ordinaryUser "chad"
    ]
```

The above configuration has factored out the part that `john`, `jane`, and
`alice` shared in common by creating a helper function named `ordinaryUser`.
This function takes a single argument named `user` and computes the record for
that user, including the public and private key paths.

We can use the `dhall-to-json` executable to convert any Dhall configuration
file into a JSON configuration file:

```console
$ dhall-to-json --pretty <<< './config0.dhall'
```
```json
[
    {
        "privateKey": "/home/john/.ssh/id_rsa",
        "publicKey": "/home/john/.ssh/id_rsa.pub",
        "user": "john"
    },
    {
        "privateKey": "/home/jane/.ssh/id_rsa",
        "publicKey": "/home/jane/.ssh/id_rsa.pub",
        "user": "jane"
    },
    {
        "privateKey": "/etc/jenkins/jenkins_rsa",
        "publicKey": "/etc/jenkins/jenkins_rsa.pub",
        "user": "jenkins"
    },
    {
        "privateKey": "/home/chad/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "chad"
    }
]
```

Now adding a new user like `alice` is a one-line change since we can reuse our
`ordinaryUser` helper function:

```dhall
-- config1.dhall

let ordinaryUser =
      \(user : Text) ->
        let privateKey = "/home/${user}/.ssh/id_rsa"

        let publicKey = "${privateKey}.pub"

        in  { privateKey, publicKey, user }

in  [ ordinaryUser "john"
    , ordinaryUser "jane"
    , { privateKey = "/etc/jenkins/jenkins_rsa"
      , publicKey = "/etc/jenkins/jenkins_rsa.pub"
      , user = "jenkins"
      }
    , ordinaryUser "chad"
    , ordinaryUser "alice"
    ]
```

We can also easily add a home directory for every ordinary user with a single
change:

```dhall
-- config2.dhall

let ordinaryUser =
      \(user : Text) ->
        let home = "/home/${user}"

        let privateKey = "${home}/.ssh/id_rsa"

        let publicKey = "${privateKey}.pub"

        in  { home, privateKey, publicKey, user }

in  [ ordinaryUser "john"
    , ordinaryUser "jane"
    , { home = "/home/jenkins"
      , privateKey = "/etc/jenkins/jenkins_rsa"
      , publicKey = "/etc/jenkins/jenkins_rsa.pub"
      , user = "jenkins"
      }
    , ordinaryUser "chad"
    , ordinaryUser "alice"
    ]
```

... and verify that the generated JSON matches what we expect:

```console
$ dhall-to-json --pretty <<< './config2.dhall'
```
```json
[
    {
        "home": "/home/john",
        "privateKey": "/home/john/.ssh/id_rsa",
        "publicKey": "/home/john/.ssh/id_rsa.pub",
        "user": "john"
    },
    {
        "home": "/home/jane",
        "privateKey": "/home/jane/.ssh/id_rsa",
        "publicKey": "/home/jane/.ssh/id_rsa.pub",
        "user": "jane"
    },
    {
        "home": "/home/jenkins",
        "privateKey": "/etc/jenkins/jenkins_rsa",
        "publicKey": "/etc/jenkins/jenkins_rsa.pub",
        "user": "jenkins"
    },
    {
        "home": "/home/chad",
        "privateKey": "/home/chad/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "chad"
    },
    {
        "home": "/home/alice",
        "privateKey": "/home/alice/.ssh/id_rsa",
        "publicKey": "/home/alice/.ssh/id_rsa.pub",
        "user": "alice"
    }
]
```

## Programmable configuration files

Dhall is not just a programming language to generate JSON files.  The
Dhall language is actually a configuration file format of its own that some
languages can read directly without converting to an intermediate JSON
representation.

We can see the resemblance to a configuration file format if we "normalize" our
Dhall expression by evaluating everything without converting to JSON.  We can do
this using the `dhall` executable which reads a Dhall expression from standard
input, evaluates the expression, and prints the result to standard output:

```console
$ dhall <<< './config2.dhall'
```
```dhall
[ { home = "/home/john"
  , privateKey = "/home/john/.ssh/id_rsa"
  , publicKey = "/home/john/.ssh/id_rsa.pub"
  , user = "john"
  }
, { home = "/home/jane"
  , privateKey = "/home/jane/.ssh/id_rsa"
  , publicKey = "/home/jane/.ssh/id_rsa.pub"
  , user = "jane"
  }
, { home = "/home/jenkins"
  , privateKey = "/etc/jenkins/jenkins_rsa"
  , publicKey = "/etc/jenkins/jenkins_rsa.pub"
  , user = "jenkins"
  }
, { home = "/home/chad"
  , privateKey = "/home/chad/.ssh/id_rsa"
  , publicKey = "/home/chad/.ssh/id_rsa.pub"
  , user = "chad"
  }
, { home = "/home/alice"
  , privateKey = "/home/alice/.ssh/id_rsa"
  , publicKey = "/home/alice/.ssh/id_rsa.pub"
  , user = "alice"
  }
]
```

The output of the `dhall` executable (i.e. the fully evaluated result) is still
a valid Dhall expression.  In fact, this is the same Dhall program except with
all of the programming language features removed, leaving behind an inert list
of records.  We can translate this normalized program to JSON and confirm that
we get the same result:

```console
$ dhall <<< './config2.dhall' | dhall-to-json --pretty
```
```json
[
    {
        "home": "/home/john",
        "privateKey": "/home/john/.ssh/id_rsa",
        "publicKey": "/home/john/.ssh/id_rsa.pub",
        "user": "john"
    },
    {
        "home": "/home/jane",
        "privateKey": "/home/jane/.ssh/id_rsa",
        "publicKey": "/home/jane/.ssh/id_rsa.pub",
        "user": "jane"
    },
    {
        "home": "/home/jenkins",
        "privateKey": "/etc/jenkins/jenkins_rsa",
        "publicKey": "/etc/jenkins/jenkins_rsa.pub",
        "user": "jenkins"
    },
    {
        "home": "/home/chad",
        "privateKey": "/home/chad/.ssh/id_rsa",
        "publicKey": "/home/chad/.ssh/id_rsa.pub",
        "user": "chad"
    },
    {
        "home": "/home/alice",
        "privateKey": "/home/alice/.ssh/id_rsa",
        "publicKey": "/home/alice/.ssh/id_rsa.pub",
        "user": "alice"
    }
]
```

You can think of the `dhall-to-json` executable as a pipeline built by chaining
the following two steps end-to-end:

* **Step 1:** `dhall-to-json` evaluates the Dhall configuration file to a normal
  form
* **Step 2:** `dhall-to-json` converts the normal form to JSON

This two-step process means that we can change the second step to generate
configuration files in other formats.

For example, this is what the `dhall-to-yaml` executable does:

* **Step 1:** `dhall-to-yaml` evaluates the Dhall configuration file to a normal
  form
* **Step 2:** `dhall-to-yaml` converts the normal form to YAML

... and here is an example of running `dhall-to-yaml` on the same
`config2.dhall` file:

```console
$ dhall-to-yaml <<< './config2.dhall'
```
```yaml
- home: /home/john
  privateKey: /home/john/.ssh/id_rsa
  publicKey: /home/john/.ssh/id_rsa.pub
  user: john
- home: /home/jane
  privateKey: /home/jane/.ssh/id_rsa
  publicKey: /home/jane/.ssh/id_rsa.pub
  user: jane
- home: /home/jenkins
  privateKey: /etc/jenkins/jenkins_rsa
  publicKey: /etc/jenkins/jenkins_rsa.pub
  user: jenkins
- home: /home/chad
  privateKey: /home/chad/.ssh/id_rsa
  publicKey: /home/chad/.ssh/id_rsa.pub
  user: chad
- home: /home/alice
  privateKey: /home/alice/.ssh/id_rsa
  publicKey: /home/alice/.ssh/id_rsa.pub
  user: alice
```

The `dhall` executable only performs the first step:

* **Step 1:** `dhall` evaluates the Dhall configuration file to a normal form

Usually we don't use the `dhall` executable to configure our programs.
Languages with Dhall bindings can read and evaluate Dhall configuration files
directly.  The `dhall` executable exists primarily for educational purposes.

## Conclusion

Now we can define what a "programmable configuration file" is:

> A programmable configuration file is a configuration file that is also an
> expression in some programming language.
>
> Evaluating a programmable configuration file reduces the file to an inert
> normal form.
>
> A programmable configuration file can be used to configure a program directly
> or translated to another configuration file format.

You might be used to thinking of configuration files as being inert data but
a "programmable configuration file" gives you access to programming language
features (like functions) and lets you translate the file an inert format by
evaluating all of the programming language features.

[JSON]: https://en.wikipedia.org/wiki/JSON
[YAML]: https://en.wikipedia.org/wiki/YAML
[XML]: https://en.wikipedia.org/wiki/XML
[TOML]: https://en.wikipedia.org/wiki/TOML
[INI]: https://en.wikipedia.org/wiki/INI_file
