![](./img/dhall-logo.png)

<head>
  <meta charset="UTF-8">
</head> 

Dhall is a programmable configuration language that is not Turing-complete

You can think of Dhall as: JSON + functions + types + imports

## Table of contents

* [Case Study](#case-study)
* [Features](#features)
* [Documentation](#documentation)
* [Overview](#overview)
    * [Interpreter](#interpreter)
    * [Language Bindings](#language-bindings)
        * [Haskell](#haskell)
        * [Nix](#nix)
    * [Compilers](#compilers)
        * [JSON and YAML](#json-and-yaml)
        * [Bash](#bash)
        * [Text](#text)
* [Design Philosophy](#design-philosophy)
* [Development Status](#development-status)
* [Name](#name)

## Case study

Let's motivate Dhall by considering the following JSON configuration
representing Haskell package metadata (wrapped to 80 columns):

```json
[
    {
        "name": "dhall",
        "author": "Gabriel Gonzalez",
        "license": "Copyright 2017 Gabriel Gonzalez\n\nRedistribution and use in
 source and binary forms, with or without\nmodification, are permitted provided 
that the following conditions are met:\n\n1. Redistributions of source code must
 retain the above copyright notice, this\n   list of conditions and the followin
g disclaimer.\n\n2. Redistributions in binary form must reproduce the above copy
right notice,\n   this list of conditions and the following disclaimer in the do
cumentation\n   and/or other materials provided with the distribution.\n\n3. Nei
ther the name of the copyright holder nor the names of its contributors\n   may 
be used to endorse or promote products derived from this software without\n   sp
ecific prior written permission.\n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT H
OLDERS AND CONTRIBUTORS \"AS IS\" AND\nANY EXPRESS OR IMPLIED WARRANTIES, INCLUD
ING, BUT NOT LIMITED TO, THE IMPLIED\nWARRANTIES OF MERCHANTABILITY AND FITNESS 
FOR A PARTICULAR PURPOSE ARE\nDISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER
 OR CONTRIBUTORS BE LIABLE\nFOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMP
LARY, OR CONSEQUENTIAL\nDAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF S
UBSTITUTE GOODS OR\nSERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRU
PTION) HOWEVER\nCAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRI
CT LIABILITY,\nOR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OU
T OF THE USE\nOF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAG
E.\n"
    },
    {
        "name": "conduit",
        "author": "Michael Snoyman",
        "license": "Copyright 2012 Michael Snoyman\n\nPermission is hereby grant
ed, free of charge, to any person obtaining a copy of this software and associat
ed documentation files (the \"Software\"), to deal in the Software without restr
iction, including without limitation the rights to use, copy, modify, merge, pub
lish, distribute, sublicense, and/or sell copies of the Software, and to permit 
persons to whom the Software is furnished to do so, subject to the following con
ditions:\n\nThe above copyright notice and this permission notice shall be inclu
ded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PR
OVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BU
T NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PUR
POSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRA
CT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE O
R THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n"
    },
    {
        "name": "async",
        "author": "Simon Marlow",
        "license": "Copyright 2012 Simon Marlow\n\nRedistribution and use in sou
rce and binary forms, with or without\nmodification, are permitted provided that
 the following conditions are met:\n\n1. Redistributions of source code must ret
ain the above copyright notice, this\n   list of conditions and the following di
sclaimer.\n\n2. Redistributions in binary form must reproduce the above copyrigh
t notice,\n   this list of conditions and the following disclaimer in the docume
ntation\n   and/or other materials provided with the distribution.\n\n3. Neither
 the name of the copyright holder nor the names of its contributors\n   may be u
sed to endorse or promote products derived from this software without\n   specif
ic prior written permission.\n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDE
RS AND CONTRIBUTORS \"AS IS\" AND\nANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 BUT NOT LIMITED TO, THE IMPLIED\nWARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
A PARTICULAR PURPOSE ARE\nDISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE\nFOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY
, OR CONSEQUENTIAL\nDAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBST
ITUTE GOODS OR\nSERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTIO
N) HOWEVER\nCAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT L
IABILITY,\nOR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 THE USE\nOF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n
"
    },
    {
        "name": "system-filepath",
        "author": "John Milikin",
        "license": "Copyright 2010 John Milikin\n\nPermission is hereby granted,
 free of charge, to any person obtaining a copy of this software and associated 
documentation files (the \"Software\"), to deal in the Software without restrict
ion, including without limitation the rights to use, copy, modify, merge, publis
h, distribute, sublicense, and/or sell copies of the Software, and to permit per
sons to whom the Software is furnished to do so, subject to the following condit
ions:\n\nThe above copyright notice and this permission notice shall be included
 in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVI
DED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT N
OT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOS
E AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIA
BLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR T
HE USE OR OTHER DEALINGS IN THE SOFTWARE.\n"
    }
]
```

---

**User:** *"How can I make the above configuration file easier to read and
edit?"*

The above configuration file is difficult to read because of the large
license texts formatted as long uninterrupted lines.  We can make the
configuration more readable by converting the JSON configuration file to a Dhall
configuration file, which supports multi-line string literals (like YAML):

```haskell
-- example0.dhall

[   {   name    = "dhall"
    ,   author  = "Gabriel Gonzalez"
    ,   license = ''
            Copyright 2017 Gabriel Gonzalez

            Redistribution and use in source and binary forms, with or without
            modification, are permitted provided that the following conditions are met:

            1. Redistributions of source code must retain the above copyright notice, this
               list of conditions and the following disclaimer.

            2. Redistributions in binary form must reproduce the above copyright notice,
               this list of conditions and the following disclaimer in the documentation
               and/or other materials provided with the distribution.

            3. Neither the name of the copyright holder nor the names of its contributors
               may be used to endorse or promote products derived from this software without
               specific prior written permission.

            THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
            ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
            WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
            DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
            FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
            DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
            SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
            CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
            OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
            OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
        ''
    }
,   {   name    = "conduit"
    ,   author  = "Michael Snoyman"
    ,   license = ''
            Copyright 2012 Michael Snoyman
            
            Permission is hereby granted, free of charge, to any person obtaining a copy of
            this software and associated documentation files (the "Software"), to deal in
            the Software without restriction, including without limitation the rights to
            use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
            of the Software, and to permit persons to whom the Software is furnished to do
            so, subject to the following conditions:
            
            The above copyright notice and this permission notice shall be included in all
            copies or substantial portions of the Software.
            
            THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
            IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
            FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
            AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
            LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
            OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
            SOFTWARE.
        ''
    }
,   {   name    = "async"
    ,   author  = "Simon Marlow"
    ,   license = ''
            Copyright 2012 Simon Marlow
            
            Redistribution and use in source and binary forms, with or without
            modification, are permitted provided that the following conditions are met:
            
            1. Redistributions of source code must retain the above copyright notice, this
               list of conditions and the following disclaimer.
            
            2. Redistributions in binary form must reproduce the above copyright notice,
               this list of conditions and the following disclaimer in the documentation
               and/or other materials provided with the distribution.
            
            3. Neither the name of the copyright holder nor the names of its contributors
               may be used to endorse or promote products derived from this software without
               specific prior written permission.
            
            THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
            ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
            WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
            DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
            FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
            DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
            SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
            CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
            OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
            OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
        ''
    }
,   {   name    = "system-filepath"
    ,   author  = "John Milikin"
    ,   license = ''
            Copyright 2010 John Milikin
            
            Permission is hereby granted, free of charge, to any person obtaining a copy of
            this software and associated documentation files (the "Software"), to deal in
            the Software without restriction, including without limitation the rights to
            use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
            of the Software, and to permit persons to whom the Software is furnished to do
            so, subject to the following conditions:
            
            The above copyright notice and this permission notice shall be included in all
            copies or substantial portions of the Software.
            
            THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
            IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
            FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
            AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
            LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
            OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
            SOFTWARE.
        ''
    }
]
```

... which we can always convert back to the original JSON configuration by
running:

```
$ dhall-to-json --pretty <<< 'example0.dhall'
[
    {
        "name": "dhall",
        "author": "Gabriel Gonzalez",
        "license": "Copyright 2017 Gabriel Gonzalez\n\nRedistribution and us..."
    },
    {
        "name": "conduit",
        "author": "Michael Snoyman",
        "license": "Copyright 2012 Michael Snoyman\n\nPermission is hereby g..."
    },
    {
        "name": "async",
        "author": "Simon Marlow",
        "license": "Copyright 2012 Simon Marlow\n\nRedistribution and use in..."
    },
    {
        "name": "system-filepath",
        "author": "John Milikin",
        "license": "Copyright 2010 John Milikin\n\nPermission is hereby gran..."
    }
]
```

---

**User:** *"I still can't tell at a glance which license is which"*

We could add comments with the name of each license (since Dhall, unlike JSON,
supports comments):

```haskell
[   {   name    = "dhall"
    ,   author  = "Gabriel Gonzalez"

        -- BSD 3-Clause
    ,   license = ''
            Copyright 2017 Gabriel Gonzalez

            Redistribution and use in source and binary forms, with or without
            ...
            OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
        ''
    }
,   {   name    = "conduit"
    ,   author  = "Michael Snoyman"

        -- MIT
    ,   license = ''
            Copyright 2012 Michael Snoyman
            
            Permission is hereby granted, free of charge, to any person obtaining a copy of
            ...
            SOFTWARE.
        ''
    }
,   {   name    = "async"
    ,   author  = "Simon Marlow"

        -- BSD 3-Clause
    ,   license = ''
            Copyright 2012 Simon Marlow
            
            Redistribution and use in source and binary forms, with or without
            ...
            OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
        ''
    }
,   {   name    = "system-filepath"
    ,   author  = "John Milikin"

        -- MIT
    ,   license = ''
            Copyright 2010 John Milikin
            
            Permission is hereby granted, free of charge, to any person obtaining a copy of
            ...
            SOFTWARE.
        ''
    }
]
```

... or we could define named functions to build each type of license, like this:

```haskell
-- example1.dhall

    let BSD-3-Clause =
        λ(args : { year : Integer, author : Text })
    →   ''
        Copyright ${Integer/show args.year} ${args.author}

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions are met:

        1. Redistributions of source code must retain the above copyright notice, this
           list of conditions and the following disclaimer.

        2. Redistributions in binary form must reproduce the above copyright notice,
           this list of conditions and the following disclaimer in the documentation
           and/or other materials provided with the distribution.

        3. Neither the name of the copyright holder nor the names of its contributors
           may be used to endorse or promote products derived from this software without
           specific prior written permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
        ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
        WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
        DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
        FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
        DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
        SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
        CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
        OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    ''

in  let MIT =
        λ(args : { year : Integer, author : Text })
    →   ''
        Copyright ${Integer/show args.year} ${args.author}
        
        Permission is hereby granted, free of charge, to any person obtaining a copy of
        this software and associated documentation files (the "Software"), to deal in
        the Software without restriction, including without limitation the rights to
        use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
        of the Software, and to permit persons to whom the Software is furnished to do
        so, subject to the following conditions:
        
        The above copyright notice and this permission notice shall be included in all
        copies or substantial portions of the Software.
        
        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
        SOFTWARE.
    ''
 
in  [   {   name    = "dhall"
        ,   author  = "Gabriel Gonzalez"
        ,   license = BSD-3-Clause { year = 2017, author = "Gabriel Gonzalez" }
        }
    ,   {   name    = "conduit"
        ,   author  = "Michael Snoyman"
        ,   license = MIT { year = 2012, author = "Michael Snoyman" }
        }
    ,   {   name    = "async"
        ,   author  = "Simon Marlow"
        ,   license = BSD-3-Clause { year = 2012, author = "Simon Marlow" }
        }
    ,   {   name    = "system-filepath"
        ,   author  = "John Milikin"
        ,   license = MIT { year = 2010, author = "John Milikin" }
        }
    ]
```

Now we can easily tell at a glance which license each package uses without the
use of comments.

---

**User:** *"I can't type Unicode characters like `'λ'` and `'→'`"*

Dhall supports ASCII equivalents of all Unicode characters, such as `\` instead
of `λ` and `->` instead of `→`.  However, this tutorial will use Unicode
everywhere because it's prettier.

---

**User:** *"The license text is still too distracting.  Could we move it out of
this configuration into another file?"*

We can move anything in Dhall (values, functions, types) into separate files and
they can refer to each other by their relative or absolute paths.  For example,
we can factor out the `MIT` and `BSD-3-Clause` functions out into separate
files, like this:

```haskell
-- BSD-3-Clause.dhall

    λ(args : { year : Integer, author : Text })
→   ''
    Copyright ${Integer/show args.year} ${args.author}

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this
       list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.

    3. Neither the name of the copyright holder nor the names of its contributors
       may be used to endorse or promote products derived from this software without
       specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
    OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
''
```

```haskell
-- MIT.dhall

    λ(args : { year : Integer, author : Text })
→   ''
    Copyright ${Integer/show args.year} ${args.author}
    
    Permission is hereby granted, free of charge, to any person obtaining a copy of
    this software and associated documentation files (the "Software"), to deal in
    the Software without restriction, including without limitation the rights to
    use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is furnished to do
    so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
''
```

... and then reference these files within our original configuration:

```haskell
-- example2.dhall

[   {   name    = "dhall"
    ,   author  = "Gabrel Gonzalez"
    ,   license = ./BSD-3-Clause.dhall { year = 2017, author = "Gabriel Gonzalez" }
    }
,   {   name    = "conduit"
    ,   author  = "Michael Snoyman"
    ,   license = ./MIT.dhall { year = 2012, author = "Michael Snoyman" }
    }
,   {   name    = "async"
    ,   author  = "Simon Marlow"
    ,   license = ./BSD-3-Clause.dhall { year = 2012, author = "Simon Marlow" }
    }
,   {   name    = "system-filepath"
    ,   author  = "John Milikin"
    ,   license = ./MIT.dhall { year = 2010, author = "John Milikin" }
    }
]
```

---

**User:** *"There's still some duplication: there is an unspoken invariant that
the package author has to match the license author"*

We can automate that away, too:

```haskell
-- example3.dhall

    let makePackage =
        λ(args : {   name        : Text
                 ,   author      : Text
                 ,   year        : Integer
                 ,   makeLicense : { year : Integer, author : Text } → Text
                 }
        )
    →   {   name    = args.name
        ,   author  = args.author
        ,   license = args.makeLicense { year = args.year, author = args.author }
        }

in  [   makePackage {   name        = "dhall"
                    ,   author      = "Gabriel Gonzalez"
                    ,   year        = 2017
                    ,   makeLicense = ./BSD-3-Clause.dhall
                    }
    ,   makePackage {   name        = "conduit"
                    ,   author      = "Michael Snoyman"
                    ,   year        = 2012
                    ,   makeLicense = ./MIT.dhall
                    }
    ,   makePackage {   name        = "async"
                    ,   author      = "Simon Marlow"
                    ,   year        = 2012
                    ,   makeLicense = ./BSD-3-Clause.dhall
                    }
    ,   makePackage {   name        = "system-filepath"
                    ,   author      = "John Milikin"
                    ,   year        = 2010
                    ,   makeLicense = ./MIT.dhall
                    }
    ]
```

---

**User:** *"Still too much duplication.  I plan on adding a lot more entries and
I don't want to type `makePackage` for every entry in the list"*

Dhall provides a Prelude of utilities to automate common tasks.  For example,
the Prelude provides a `map` function that transforms every element of the list
with the same function (such as `makePackage`):

```haskell
-- example4.dhall

    let map = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/map

in  let makePackage =
        λ(args : {   name        : Text
                 ,   author      : Text
                 ,   year        : Integer
                 ,   makeLicense : { year : Integer, author : Text } → Text
                 }
        )
    →   {   name    = args.name
        ,   author  = args.author
        ,   license = args.makeLicense { year = args.year, author = args.author }
        }

in  map

    {   name        : Text
    ,   author      : Text
    ,   year        : Integer
    ,   makeLicense : { year : Integer, author : Text } → Text
    }

    {   name    : Text
    ,   author  : Text
    ,   license : Text
    }

    makePackage

    [   {   name        = "dhall"
        ,   author      = "Gabriel Gonzalez"
        ,   year        = 2017
        ,   makeLicense = ./BSD-3-Clause.dhall
        }
    ,   {   name        = "conduit"
        ,   author      = "Michael Snoyman"
        ,   year        = 2012
        ,   makeLicense = ./MIT.dhall
        }
    ,   {   name        = "async"
        ,   author      = "Simon Marlow"
        ,   year        = 2012
        ,   makeLicense = ./BSD-3-Clause.dhall
        }
    ,   {   name        = "system-filepath"
        ,   author      = "John Milikin"
        ,   year        = 2010
        ,   makeLicense = ./MIT.dhall
        }
    ]
```

---

**User:** *"How am I supposed to even know that there is a `map` function or
what arguments the function expects?"*

We can browse the Prelude online here:

* [Prelude](https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude)

... and every function has documentation, examples, and a type signature:

```bash
$ curl https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/map
{-
Tranform a list by applying a function to each element

Examples:

./map Natural Bool Natural/even ([+2, +3, +5] : List Natural)
= [True, False, False] : List Bool

./map Natural Bool Natural/even ([] : List Natural)
= [] : List Bool
-}
let map : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
    =   λ(a : Type)
    →   λ(b : Type)
    →   λ(f : a → b)
    →   λ(xs : List a)
    →   List/build
        b
        (   λ(list : Type)
        →   λ(cons : b → list → list)
        →   List/fold a xs list (λ(x : a) → cons (f x))
        )

in  map
```

The type signature for `map`:

```haskell
let map : ∀(a : Type) → ∀(b : Type) → (a → b) → List a → List b
```

... says that `map` takes four arguments:

* The first argument is the element type of the input list
* The second argument is the element type of the output list
* The third argument is the function to apply to each element of the list
* The fourt argument is the input list

... and the result is the output list

---

**User:** *"These types are still long and repetitive.  One type shows up twice
in the source code"*

We can import types just like anything else in Dhall:

```haskell
-- Input.dhall
{   name        : Text
,   author      : Text
,   year        : Integer
,   makeLicense : { year : Integer, author : Text } → Text
}
```

```haskell
-- Output.dhall
{   name    : Text
,   author  : Text
,   license : Text
}
```

```haskell
-- example5.dhall

    let map = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/map

in  let makePackage =
        λ(args : ./Input.dhall)
    →   {   name    = args.name
        ,   author  = args.author
        ,   license = args.makeLicense { year = args.year, author = args.author }
        }

in  map ./Input.dhall ./Output.dhall makePackage

    [   {   name        = "dhall"
        ,   author      = "Gabriel Gonzalez"
        ,   year        = 2017
        ,   makeLicense = ./BSD-3-Clause.dhall
        }
    ,   {   name        = "conduit"
        ,   author      = "Michael Snoyman"
        ,   year        = 2012
        ,   makeLicense = ./MIT.dhall
        }
    ,   {   name        = "async"
        ,   author      = "Simon Marlow"
        ,   year        = 2012
        ,   makeLicense = ./BSD-3-Clause.dhall
        }
    ,   {   name        = "system-filepath"
        ,   author      = "John Milikin"
        ,   year        = 2010
        ,   makeLicense = ./MIT.dhall
        }
    ]
```

---

**User:** *"Why doesn't Dhall use JSON-like syntax?"*

JSON isn't the only file format that Dhall supports.  For example, we can
convert our Dhall configuration to YAML:

```bash
$ dhall-to-yaml <<< '/tmp/test/example3'
- name: dhall
  author: Gabriel Gonzalez
  license: ! '
    Copyright 2017 Gabriel Gonzalez


    Redistribution and use in source and binary forms, with or without modification,
    ...
    OF THE POSSIBILITY OF SUCH DAMAGE.

'
- name: conduit
  author: Michael Snoyman
  license: ! '
    Copyright 2012 Michael Snoyman

    Permission is hereby granted, free of charge, to any person obtaining a copy of
    ...
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

'
- name: async
  author: Simon Marlow
  license: ! '
    Copyright 2012 Simon Marlow

    Redistribution and use in source and binary forms, with or without modification,
    ...
    OF THE POSSIBILITY OF SUCH DAMAGE.

'
- name: system-filepath
  author: John Milikin
  license: ! '

    Copyright 2010 John Milikin


    Permission is hereby granted, free of charge, to any person obtaining a copy of
    ...
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

'
```

... or read our Dhall configuration directly into some programming languages
(like Haskell) without going through a JSON intermediate:

```haskell
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Dhall

data Package = Package
    { name    :: Text
    , author  :: Text
    , license :: Text
    } deriving (Generic, Interpret, Show)

main :: IO ()
main = do
    packages <- input auto "./example.dhall"
    print (packages :: Vector Package)
```

---

**User:** *"Why not just always go through a JSON intermediate?  Every language
already supports JSON"*

You can load more exotic things into the language (like functions) if the
language integrates directly with Dhall:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Dhall

main :: IO ()
main = do
    twice <- input auto "λ(x : Integer) → [x, x]" :: IO (Integer -> Vector Integer)
    print (twice 5) -- prints: "[5,5]"
```

... and other features, too, like sum types

---

**User:** *"What's a sum type?"*

At this point you should read the [language manual](dhall-haskell-tutorial) ☺

Or you can continue reading to learn about Dhall's feature set

## Features:

* Total - Evaluation always terminates and never hangs or infinitely loops
* Safe - Evaluation never crashes or throws exceptions
* Distributed - Expressions can reference other expressions by URL or path
* Strongly normalizing - All expressions (even functions) have a normal form
* Statically typed - Configurations can be validated ahead-of-time
* Strongly typed - No coercions, casts or subtyping
* Built-in data types - Includes lists, anonymous records and anonymous unions

## Documentation

The Dhall language originated as a Haskell-specific configuration file format
and is expanding to support more languages and file formats.  Consequently, the
Haskell package for Dhall still hosts the official tutorial and language manual:

* [Dhall tutorial and manual][dhall-haskell-tutorial]

... which will eventually become a language-agnostic tutorial

You can also read about the original motivation behind the language here:

* [Dhall - A non-Turing-complete configuration language][dhall-haskell-post]

Finally, we have a cheatsheet for a very condensed overview and quick lookup:

* [Dhall Cheatsheet](./cheatsheet.md)

## Overview

You can use Dhall in one of three ways:

*   **Interpreter:** You can install a language-independent command-line program
    that can import, type-check and evaluate Dhall expressions
*   **Language binding:** You can use a language-specific library to load
    Dhall configuration files directly into your programs
*   **Compilers:** You can compile Dhall expressions to several common
    configuration file formats using command-line utilities

I recommend the following progression for adopting Dhall:

* Install the interpreter to learn more about the language
* If you like what you see, then:
    * If your preferred programming language supports Dhall:
        * Use the language binding to configure your programs using Dhall
    * If your language does not yet support Dhall
        * Compile Dhall to a supported configuration format (such as JSON)

The following sections tour each of these use cases in more detail

### Interpreter

You can install a Dhall interpreter that can type-check and evaluate Dhall
expressions from the command line.  You can use this interpreter to:

*   **Learn how the language works:**

    ```bash
    $ dhall <<< 'True && False'
    Bool

    False
    ```

    The first line of output shows the inferred type of an expression:

    ```bash
    $ dhall <<< 'List/length'
    ∀(a : Type) → List a → Natural

    List/length
    ```

    The second line of output shows the fully evaluated normal form of an
    expression:

    ```bash
    $ dhall <<< 'λ(x : Text) → let y = True in if y != False then x else "?"'
    ∀(x : Text) → Text

    λ(x : Text) → x
    ```

*   **Validate a configuration file ahead of time against a schema:**

    ```bash
    $ cat config
    { foo = List/length Integer [2, 3, 5], bar = True && False }
    ```

    ```bash
    $ cat schema
    { foo : Natural, bar : Bool }
    ```

    Dhall lets you import expressions and types by their path:

    ```bash
    $ dhall <<< './config : ./schema'
    { bar : Bool, foo : Natural }

    { bar = False, foo = +3 }
    ```

    Schema validation is the same thing as a type annotation

*   **Detect type errors:**

    ```bash
    $ dhall <<< 'λ(x : Integer) → x && True'

    Use "dhall --explain" for detailed errors

    Error: ❰&&❱ only works on ❰Bool❱s

    x && True

    (stdin):1:18
    ```

    You can ask the type checker to go into more detail using the `--explain`
    flag:

    ```bash
    $ dhall --explain <<< 'λ(x : Integer) → x && True'
    
    
    x : Integer
    
    Error: ❰&&❱ only works on ❰Bool❱s
    
    Explanation: The ❰&&❱ operator expects two arguments that have type ❰Bool❱
                                                                                    
    For example, this is a valid use of ❰&&❱:                           
                                                                                    
                                                                                    
        ┌───────────────┐                                                           
        │ True && False │                                               
        └───────────────┘                                                           
                                                                                    
                                                                                    
    You provided this argument:                                                     
                                                                                    
    ↳ x                                                                
                                                                                    
    ... which does not have type ❰Bool❱ but instead has type:                       
                                                                                    
    ↳ Integer                                                                
    
    ────────────────────────────────────────────────────────────────────────────────
    
    x && True
    
    (stdin):1:18
    ```

*   **Resolve remote expressions:**

    ```bash
    dhall <<< 'https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/replicate'

    ∀(n : Natural) → ∀(a : Type) → ∀(x : a) → List a

    λ(n : Natural) → λ(a : Type) → λ(x : a) → List/build a (λ(list : Type) → λ(cons : a → list → list) → Natural/fold n list (cons x))
    ```

    You can import arbitrary expressions URL, too.  In fact, the Dhall Prelude
    is hosted this way:

    * [Dhall Prelude][dhall-prelude]

*   **Reduce an expression to normal form:**

    ```bash
    $ cat ./example
        let replicate = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/replicate

    in  let exclaim = λ(t : Text) → t ++ "!"

    in  λ(x : Text) → replicate +3 Text (exclaim x)
    ```

    You can reduce functions to normal form, even when they haven't been
    applied to all of their arguments

    ```bash
    $ dhall <<< './example'
    ∀(x : Text) → List Text

    λ(x : Text) → [x ++ "!", x ++ "!", x ++ "!"] : List Text
    ```

    The normal form is equivalent to the original program except stripped of
    all imports and indirection

Learn more:

* [GitHub repository][dhall-haskell]
* [Language guide][dhall-haskell-tutorial]
* [Blog post][dhall-haskell-post]

### Language Bindings

You can use Dhall to configure programs written in other languages. This is the
most common use case for Dhall: a type-safe and non-Turing-complete
configuration language 

Dhall currently supports two complete language bindings:

* [Haskell][dhall-haskell]
* [Nix][dhall-nix]

... and two additional language bindings in progress:

* [Scala][dhall-scala]
* [Rust][dhall-rust]

#### Haskell

You can load Dhall expressions into Haskell using the `input` function:

```haskell
>>> input auto "True && False' :: IO Bool
False
```

The compiler validates the expression against the expected type:

```haskell
>>> input auto "1" :: IO Bool
*** Exception: 
Error: Expression doesn't match annotation

1 : Bool

(input):1:1
```

You can even marshall some Dhall functions into Haskell:

```haskell
>>> twice <- input auto "λ(x : Integer) → [x, x]" :: IO (Integer -> [Integer])
>>> twice 5
[5,5]
```

You can also decode Dhall expressions into Haskell types that derive
`Generic`:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Dhall

data Example = Example { foo :: Integer, bar :: Vector Double }
    deriving (Generic, Show)

instance Interpret Example

main :: IO ()
main = do
    x <- input auto "./config"
    print (x :: Example)
```

... which reads in this configuration file:

```bash
$ cat ./config
{ foo = 1
, bar = [3.0, 4.0, 5.0]
}
```

... and prints the configuration:

```bash
$ ./example
Example {foo = 1, bar = [3.0,4.0,5.0]}
```

Learn more:

* [GitHub repository][dhall-haskell]
* [Tutorial][dhall-haskell-tutorial]

#### Nix

`nixpkgs` provides a `dhallToNix` utility which you can use to translate a
Dhall expression to the corresponding Nix expression.  This lets you carve out
a typed and total subset of Nix

For example, this Dhall expression:

```nix
let
  pkgs = import <nixpkgs> { };

in
  pkgs.dhallToNix ''
    { foo = λ(x : Bool) → [x, x]
    , bar = Natural/even +2
    , baz = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/Text/concat
    }
  ''
```

... translates to this Nix expression

```nix
{ foo = x : [ x x ];
  bar = true;
  baz = xs:
    (t: xs: t: cons:
      builtins.foldl' (f: y: ys: f (cons y ys)) (ys: ys) xs
    ) {} xs {} (x: y: x + y) "";
}
```

Learn more:

* [`dhallToNix` function][dhallToNix]
* [GitHub repository][dhall-nix]
* [Tutorial][dhall-nix-tutorial]
* [Blog post][dhall-nix-post]

### Compilers

You can compile Dhall expressions to other configuration formats, such as:

* [JSON and YAML][dhall-json]
* [Bash][dhall-bash] declarations
* [Text][dhall-text] (i.e. Dhall as a template engine)

These compilers can only translate a subset of Dhall to these other formats.
For example, you cannot translate Dhall functions to JSON

#### JSON and YAML

The `dhall-to-json` executable lets you compile Dhall expressions to JSON:

```bash
$ dhall-to-json <<< '{ foo = 1, bar = True, baz = [1, 2, 3] }'
{"foo":1,"baz":[1,2,3],"bar":true}
```

YAML is already a superset of JSON, but the `dhall-to-yaml` executable lets you
compile Dhall expressions to idiomatic YAML syntax, too:

```bash
dhall-to-yaml <<< '{ foo = 1, bar = True, baz = [1, 2, 3] }'
foo: 1
baz:
- 1
- 2
- 3
bar: true
```

Some configuration file formats are supersets of JSON such as the
[HashiCorp configuration language][hcl] (Used to configure `terraform`) so you
can compile Dhall expressions to these configuration file formats, too.

Learn more:

* [GitHub repository][dhall-json]
* [Tutorial][dhall-json-tutorial]
* [Blog post][dhall-json-post]

#### Bash

You can also compile Dhall expressions to either Bash expressions:

```bash
$ dhall-to-bash <<< 'List/length Bool [True, False]'
2
```

... or Bash declarations:

```bash
$ dhall-to-bash --declare FOO <<< '{ bar = 1, baz = "ABC" }'
declare -r -A FOO=([bar]=1 [baz]=ABC)
$ eval $(dhall-to-bash --declare FOO <<< '{ bar = 1, baz = "ABC" }')
$ echo ${FOO[bar]}
1
$ echo ${FOO[baz]}
ABC
```

```bash

```

Learn more:

* [GitHub repository][dhall-bash]
* [Tutorial][dhall-bash-tutorial]
* [Blog post][dhall-bash-post]

#### Text

You can also use the `dhall-to-text` executable as a template engine

For example, given this template:

```bash
$ cat function
    λ(record : { name        : Text
               , value       : Double
               , taxed_value : Double
               , in_ca       : Bool
               }
     ) → ''
Hello ${record.name}
You have just won ${Double/show record.value} dollars!
${ if record.in_ca
   then "Well, ${Double/show record.taxed_value} dollars, after taxes"
   else ""
 }
''
```

... and this payload:

```bash
$ cat value
{ name        = "Chris"
, value       = 10000.0
, taxed_value = 6000.0
, in_ca       = True
}
```

... you can apply the template to the payload and render the result using the
`dhall-to-text` executable:

```bash
$ dhall-to-text <<< './function ./value'
Hello Chris
You have just won 10000.0 dollars!
Well, 6000.0 dollars, after taxes
```

Learn more:

* [GitHub repository and tutorial][dhall-text]
* [Blog post][dhall-text-post]

## Design philosophy

Programming languages are all about design tradeoffs and the Dhall language uses
the following guiding principles (in order of descending priority) that help
navigate those tradeoffs:

* Polish

    The language should delight users.  Error messages should be fantastic,
    execution should be snappy, documentation should be excellent, and
    everything should "just work".

* Simplicity

    When in doubt, cut it out.  Every configuration language needs bindings to
    multiple programming languages, and the more complex the configuration
    language the more difficult to create new bindings.  Let the host language
    that you bind to compensate for any missing features from Dhall.

* Beginner-friendliness

    Dhall needs to be a language that anybody can learn in a day and debug
    with little to no assistance from others.  Otherwise people can't recommend
    Dhall to their team with confidence.

* Robustness

    A configuration language needs to be rock solid.  The last thing a person
    wants to debug is their configuration file.  The language should never hang
    or crash.  Ever.

* Consistency

    There should only be one way to do something.  Users should be able to
    instantly discern whether or not something is possible within the Dhall
    language or not.

The Dhall configuration language is also designed to negate many of the common
objections to programmable configuration files, such as:

> "Config files shouldn't be Turing complete"

Dhall is not Turing-complete.  Evaluation always terminates, no exceptions

> "Configuration languages become unreadable due to abstraction and indirection"

Every Dhall configuration file can be reduced to a normal form which eliminates
all abstraction and indirection

> "Users will go crazy with syntax and user-defined constructs"

Dhall is a very minimal programming language.  For example: you cannot even
compare strings for equality.  The language also forbids many other common
operations in order to force users to keep things simple

The biggest issue with using Dhall as a configuration language is that there are
currently only Haskell bindings.  If you would like to contribute bindings to
another language then go for it, otherwise I will do my best to contribute them
as time permits.

## Development status

I am beginning to author a formal language standard for Dhall to help with
porting Dhall to other languages.  If you would like to assist with either
standardizing the language or creating new bindings just let me know through the
issue tracker.

## Name

The language is named after a
[Dustman from the game Planescape: Torment][dhall-name]
who belongs to a faction obsessed with death (termination).  The logo represents
his quill and inkwell

[dhall-haskell]: https://github.com/dhall-lang/dhall-haskell
[dhall-haskell-tutorial]: https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html
[dhall-haskell-post]: http://www.haskellforall.com/2016/12/dhall-non-turing-complete-configuration.html
[dhall-nix]: https://github.com/dhall-lang/dhall-nix
[dhall-nix-tutorial]: https://hackage.haskell.org/package/dhall-nix/docs/Dhall-Nix.html
[dhall-nix-post]: http://www.haskellforall.com/2017/01/typed-nix-programming-using-dhall.html
[dhall-scala]: https://github.com/amarpotghan/dhall-scala
[dhall-rust]: https://github.com/nanotech/dhall-rs
[dhall-json]: https://github.com/dhall-lang/dhall-json
[dhall-json-tutorial]: https://hackage.haskell.org/package/dhall-json/docs/Dhall-JSON.html
[dhall-json-post]: http://www.haskellforall.com/2017/02/program-json-and-yaml-with-dhall.html
[dhall-bash]: https://github.com/dhall-lang/dhall-bash
[dhall-bash-tutorial]: https://hackage.haskell.org/package/dhall-bash/docs/Dhall-Bash.html
[dhall-bash-post]: http://www.haskellforall.com/2017/04/use-dhall-to-configure-bash-programs.html
[dhall-text]: https://github.com/dhall-lang/dhall-text
[dhall-text-post]: http://www.haskellforall.com/2017/06/dhall-is-now-template-engine.html
[dhallToNix]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/dhall-to-nix.nix
[dhall-name]: http://torment.wikia.com/wiki/Dhall
[dhall-prelude]: https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude
[hcl]: https://github.com/hashicorp/hcl
