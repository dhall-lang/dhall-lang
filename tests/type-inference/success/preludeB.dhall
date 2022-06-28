{ Bool :
    { and : ∀(xs : List Bool) → Bool
    , build :
        ∀(f : ∀(bool : Type) → ∀(true : bool) → ∀(false : bool) → bool) → Bool
    , even : ∀(xs : List Bool) → Bool
    , fold :
        ∀(b : Bool) → ∀(bool : Type) → ∀(true : bool) → ∀(false : bool) → bool
    , not : ∀(b : Bool) → Bool
    , odd : ∀(xs : List Bool) → Bool
    , or : ∀(xs : List Bool) → Bool
    , show : ∀(b : Bool) → Text
    }
, Double : { show : Double → Text }
, Function :
    { compose :
        ∀(A : Type) →
        ∀(B : Type) →
        ∀(C : Type) →
        ∀(f : A → B) →
        ∀(g : B → C) →
        ∀(x : A) →
          C
    , identity : ∀(a : Type) → ∀(x : a) → a
    }
, Integer :
    { abs : ∀(n : Integer) → Natural
    , add : ∀(m : Integer) → ∀(n : Integer) → Integer
    , clamp : Integer → Natural
    , equal : ∀(a : Integer) → ∀(b : Integer) → Bool
    , greaterThan : ∀(x : Integer) → ∀(y : Integer) → Bool
    , greaterThanEqual : ∀(x : Integer) → ∀(y : Integer) → Bool
    , lessThan : ∀(x : Integer) → ∀(y : Integer) → Bool
    , lessThanEqual : ∀(x : Integer) → ∀(y : Integer) → Bool
    , multiply : ∀(m : Integer) → ∀(n : Integer) → Integer
    , negate : Integer → Integer
    , negative : ∀(n : Integer) → Bool
    , nonNegative : ∀(n : Integer) → Bool
    , nonPositive : ∀(n : Integer) → Bool
    , positive : ∀(n : Integer) → Bool
    , show : Integer → Text
    , subtract : ∀(m : Integer) → ∀(n : Integer) → Integer
    , toDouble : Integer → Double
    , toNatural : ∀(n : Integer) → Optional Natural
    }
, JSON :
    { Nesting : Type
    , Tagged : ∀(a : Type) → Type
    , Type : Type
    , array :
        ∀ ( x
          : List
              ( ∀(JSON : Type) →
                ∀ ( json
                  : { array : List JSON → JSON
                    , bool : Bool → JSON
                    , double : Double → JSON
                    , integer : Integer → JSON
                    , null : JSON
                    , object : List { mapKey : Text, mapValue : JSON } → JSON
                    , string : Text → JSON
                    }
                  ) →
                  JSON
              )
          ) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , bool :
        ∀(x : Bool) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , double :
        ∀(x : Double) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , integer :
        ∀(x : Integer) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , keyText :
        ∀(key : Text) → ∀(value : Text) → { mapKey : Text, mapValue : Text }
    , keyValue :
        ∀(v : Type) →
        ∀(key : Text) →
        ∀(value : v) →
          { mapKey : Text, mapValue : v }
    , natural :
        ∀(x : Natural) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , null :
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , number :
        ∀(x : Double) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , object :
        ∀ ( x
          : List
              { mapKey : Text
              , mapValue :
                  ∀(JSON : Type) →
                  ∀ ( json
                    : { array : List JSON → JSON
                      , bool : Bool → JSON
                      , double : Double → JSON
                      , integer : Integer → JSON
                      , null : JSON
                      , object : List { mapKey : Text, mapValue : JSON } → JSON
                      , string : Text → JSON
                      }
                    ) →
                    JSON
              }
          ) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , omitNullFields :
        ∀ ( old
          : ∀(JSON : Type) →
            ∀ ( json
              : { array : List JSON → JSON
                , bool : Bool → JSON
                , double : Double → JSON
                , integer : Integer → JSON
                , null : JSON
                , object : List { mapKey : Text, mapValue : JSON } → JSON
                , string : Text → JSON
                }
              ) →
              JSON
          ) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , render :
        ∀ ( json
          : ∀(JSON : Type) →
            ∀ ( json
              : { array : List JSON → JSON
                , bool : Bool → JSON
                , double : Double → JSON
                , integer : Integer → JSON
                , null : JSON
                , object : List { mapKey : Text, mapValue : JSON } → JSON
                , string : Text → JSON
                }
              ) →
              JSON
          ) →
          Text
    , renderCompact :
        ∀ ( j
          : ∀(JSON : Type) →
            ∀ ( json
              : { array : List JSON → JSON
                , bool : Bool → JSON
                , double : Double → JSON
                , integer : Integer → JSON
                , null : JSON
                , object : List { mapKey : Text, mapValue : JSON } → JSON
                , string : Text → JSON
                }
              ) →
              JSON
          ) →
          Text
    , renderInteger : ∀(integer : Integer) → Text
    , renderYAML :
        ∀ ( json
          : ∀(JSON : Type) →
            ∀ ( json
              : { array : List JSON → JSON
                , bool : Bool → JSON
                , double : Double → JSON
                , integer : Integer → JSON
                , null : JSON
                , object : List { mapKey : Text, mapValue : JSON } → JSON
                , string : Text → JSON
                }
              ) →
              JSON
          ) →
          Text
    , string :
        ∀(x : Text) →
        ∀(JSON : Type) →
        ∀ ( json
          : { array : List JSON → JSON
            , bool : Bool → JSON
            , double : Double → JSON
            , integer : Integer → JSON
            , null : JSON
            , object : List { mapKey : Text, mapValue : JSON } → JSON
            , string : Text → JSON
            }
          ) →
          JSON
    , tagInline :
        ∀(tagFieldName : Text) →
        ∀(a : Type) →
        ∀(contents : a) →
          { contents : a, field : Text, nesting : < Inline | Nested : Text > }
    , tagNested :
        ∀(contentsFieldName : Text) →
        ∀(tagFieldName : Text) →
        ∀(a : Type) →
        ∀(contents : a) →
          { contents : a, field : Text, nesting : < Inline | Nested : Text > }
    }
, List :
    { all : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : List a) → Bool
    , any : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : List a) → Bool
    , build :
        ∀(a : Type) →
        (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list) →
          List a
    , concat : ∀(a : Type) → ∀(xss : List (List a)) → List a
    , concatMap :
        ∀(a : Type) → ∀(b : Type) → ∀(f : a → List b) → ∀(xs : List a) → List b
    , default : ∀(a : Type) → ∀(o : Optional (List a)) → List a
    , drop : Natural → ∀(a : Type) → List a → List a
    , empty : ∀(a : Type) → List a
    , filter : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : List a) → List a
    , filterMap :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(f : a → Optional b) →
        ∀(xs : List a) →
          List b
    , fold :
        ∀(a : Type) →
        List a →
        ∀(list : Type) →
        ∀(cons : a → list → list) →
        ∀(nil : list) →
          list
    , foldLeft :
        ∀(a : Type) →
        ∀(xs : List a) →
        ∀(list : Type) →
        ∀(cons : list → a → list) →
        ∀(nil : list) →
          list
    , generate : ∀(n : Natural) → ∀(a : Type) → ∀(f : Natural → a) → List a
    , head : ∀(a : Type) → List a → Optional a
    , index : ∀(n : Natural) → ∀(a : Type) → ∀(xs : List a) → Optional a
    , indexed : ∀(a : Type) → List a → List { index : Natural, value : a }
    , iterate : ∀(n : Natural) → ∀(a : Type) → ∀(f : a → a) → ∀(x : a) → List a
    , last : ∀(a : Type) → List a → Optional a
    , length : ∀(a : Type) → List a → Natural
    , map : ∀(a : Type) → ∀(b : Type) → ∀(f : a → b) → ∀(xs : List a) → List b
    , null : ∀(a : Type) → ∀(xs : List a) → Bool
    , partition :
        ∀(a : Type) →
        ∀(f : a → Bool) →
        ∀(xs : List a) →
          { false : List a, true : List a }
    , replicate : ∀(n : Natural) → ∀(a : Type) → ∀(x : a) → List a
    , reverse : ∀(a : Type) → List a → List a
    , shifted :
        ∀(a : Type) →
        ∀(kvss : List (List { index : Natural, value : a })) →
          List { index : Natural, value : a }
    , take : Natural → ∀(a : Type) → List a → List a
    , unpackOptionals : ∀(a : Type) → ∀(xs : List (Optional a)) → List a
    , unzip :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(xs : List { _1 : a, _2 : b }) →
          { _1 : List a, _2 : List b }
    , zip :
        ∀(a : Type) →
        ∀(xs : List a) →
        ∀(b : Type) →
        ∀(ys : List b) →
          List { _1 : a, _2 : b }
    }
, Location : { Type : Type }
, Map :
    { Entry : ∀(k : Type) → ∀(v : Type) → Type
    , Type : ∀(k : Type) → ∀(v : Type) → Type
    , empty : ∀(k : Type) → ∀(v : Type) → List { mapKey : k, mapValue : v }
    , keyText :
        ∀(key : Text) → ∀(value : Text) → { mapKey : Text, mapValue : Text }
    , keyValue :
        ∀(v : Type) →
        ∀(key : Text) →
        ∀(value : v) →
          { mapKey : Text, mapValue : v }
    , keys :
        ∀(k : Type) →
        ∀(v : Type) →
        ∀(xs : List { mapKey : k, mapValue : v }) →
          List k
    , map :
        ∀(k : Type) →
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(f : a → b) →
        ∀(m : List { mapKey : k, mapValue : a }) →
          List { mapKey : k, mapValue : b }
    , unpackOptionals :
        ∀(k : Type) →
        ∀(v : Type) →
        ∀(xs : List { mapKey : k, mapValue : Optional v }) →
          List { mapKey : k, mapValue : v }
    , values :
        ∀(k : Type) →
        ∀(v : Type) →
        ∀(xs : List { mapKey : k, mapValue : v }) →
          List v
    }
, Monoid : ∀(m : Type) → Type
, Natural :
    { build :
        ( ∀(natural : Type) →
          ∀(succ : natural → natural) →
          ∀(zero : natural) →
            natural
        ) →
          Natural
    , enumerate : ∀(n : Natural) → List Natural
    , equal : ∀(a : Natural) → ∀(b : Natural) → Bool
    , even : Natural → Bool
    , fold :
        Natural →
        ∀(natural : Type) →
        ∀(succ : natural → natural) →
        ∀(zero : natural) →
          natural
    , greaterThan : ∀(x : Natural) → ∀(y : Natural) → Bool
    , greaterThanEqual : ∀(x : Natural) → ∀(y : Natural) → Bool
    , isZero : Natural → Bool
    , lessThan : ∀(x : Natural) → ∀(y : Natural) → Bool
    , lessThanEqual : ∀(x : Natural) → ∀(y : Natural) → Bool
    , listMax : ∀(xs : List Natural) → Optional Natural
    , listMin : ∀(xs : List Natural) → Optional Natural
    , max : ∀(a : Natural) → ∀(b : Natural) → Natural
    , min : ∀(a : Natural) → ∀(b : Natural) → Natural
    , odd : Natural → Bool
    , product : ∀(xs : List Natural) → Natural
    , show : Natural → Text
    , sort : ∀(xs : List Natural) → List Natural
    , subtract : Natural → Natural → Natural
    , sum : ∀(xs : List Natural) → Natural
    , toDouble : ∀(n : Natural) → Double
    , toInteger : Natural → Integer
    }
, NonEmpty :
    { Type : ∀(a : Type) → Type
    , all :
        ∀(a : Type) →
        ∀(f : a → Bool) →
        ∀(xs : { head : a, tail : List a }) →
          Bool
    , any :
        ∀(a : Type) →
        ∀(f : a → Bool) →
        ∀(xs : { head : a, tail : List a }) →
          Bool
    , concat :
        ∀(a : Type) →
        ∀ ( xss
          : { head : { head : a, tail : List a }
            , tail : List { head : a, tail : List a }
            }
          ) →
          { head : a, tail : List a }
    , concatMap :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(f : a → { head : b, tail : List b }) →
        ∀(xs : { head : a, tail : List a }) →
          { head : b, tail : List b }
    , head : ∀(a : Type) → ∀(xs : { head : a, tail : List a }) → a
    , index :
        ∀(n : Natural) →
        ∀(a : Type) →
        ∀(xs : { head : a, tail : List a }) →
          Optional a
    , indexed :
        ∀(a : Type) →
        ∀(xs : { head : a, tail : List a }) →
          { head : { index : Natural, value : a }
          , tail : List { index : Natural, value : a }
          }
    , last : ∀(a : Type) → ∀(xs : { head : a, tail : List a }) → a
    , length : ∀(a : Type) → ∀(xs : { head : a, tail : List a }) → Natural
    , make :
        ∀(a : Type) →
        ∀(head : a) →
        ∀(tail : List a) →
          { head : a, tail : List a }
    , map :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(f : a → b) →
        ∀(xs : { head : a, tail : List a }) →
          { head : b, tail : List b }
    , reverse :
        ∀(a : Type) →
        ∀(xs : { head : a, tail : List a }) →
          { head : a, tail : List a }
    , shifted :
        ∀(a : Type) →
        ∀ ( kvss
          : { head :
                { head : { index : Natural, value : a }
                , tail : List { index : Natural, value : a }
                }
            , tail :
                List
                  { head : { index : Natural, value : a }
                  , tail : List { index : Natural, value : a }
                  }
            }
          ) →
          { head : { index : Natural, value : a }
          , tail : List { index : Natural, value : a }
          }
    , singleton : ∀(a : Type) → ∀(x : a) → { head : a, tail : List a }
    , toList : ∀(a : Type) → ∀(xs : { head : a, tail : List a }) → List a
    , unzip :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(xs : { head : { _1 : a, _2 : b }, tail : List { _1 : a, _2 : b } }) →
          { _1 : { head : a, tail : List a }, _2 : { head : b, tail : List b } }
    , zip :
        ∀(a : Type) →
        ∀(xs : { head : a, tail : List a }) →
        ∀(b : Type) →
        ∀(ys : { head : b, tail : List b }) →
          { head : { _1 : a, _2 : b }, tail : List { _1 : a, _2 : b } }
    }
, Operator :
    { `!=` : ∀(m : Bool) → ∀(n : Bool) → Bool
    , `#` : ∀(type : Type) → ∀(m : List type) → ∀(n : List type) → List type
    , `&&` : ∀(m : Bool) → ∀(n : Bool) → Bool
    , `*` : ∀(m : Natural) → ∀(n : Natural) → Natural
    , `+` : ∀(m : Natural) → ∀(n : Natural) → Natural
    , `++` : ∀(m : Text) → ∀(n : Text) → Text
    , `==` : ∀(m : Bool) → ∀(n : Bool) → Bool
    , `||` : ∀(m : Bool) → ∀(n : Bool) → Bool
    }
, Optional :
    { all : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : Optional a) → Bool
    , any : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : Optional a) → Bool
    , build :
        ∀(a : Type) →
        ∀ ( build
          : ∀(optional : Type) →
            ∀(some : a → optional) →
            ∀(none : optional) →
              optional
          ) →
          Optional a
    , concat : ∀(a : Type) → ∀(x : Optional (Optional a)) → Optional a
    , concatMap :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(f : a → Optional b) →
        ∀(o : Optional a) →
          Optional b
    , default : ∀(a : Type) → ∀(default : a) → ∀(o : Optional a) → a
    , filter : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : Optional a) → Optional a
    , fold :
        ∀(a : Type) →
        ∀(o : Optional a) →
        ∀(optional : Type) →
        ∀(some : a → optional) →
        ∀(none : optional) →
          optional
    , head : ∀(a : Type) → ∀(xs : List (Optional a)) → Optional a
    , last : ∀(a : Type) → ∀(xs : List (Optional a)) → Optional a
    , length : ∀(a : Type) → ∀(xs : Optional a) → Natural
    , map :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(f : a → b) →
        ∀(o : Optional a) →
          Optional b
    , null : ∀(a : Type) → ∀(xs : Optional a) → Bool
    , toList : ∀(a : Type) → ∀(o : Optional a) → List a
    , unzip :
        ∀(a : Type) →
        ∀(b : Type) →
        ∀(xs : Optional { _1 : a, _2 : b }) →
          { _1 : Optional a, _2 : Optional b }
    }
, Text :
    { concat : ∀(xs : List Text) → Text
    , concatMap : ∀(a : Type) → ∀(f : a → Text) → ∀(xs : List a) → Text
    , concatMapSep :
        ∀(separator : Text) →
        ∀(a : Type) →
        ∀(f : a → Text) →
        ∀(elements : List a) →
          Text
    , concatSep : ∀(separator : Text) → ∀(elements : List Text) → Text
    , default : ∀(o : Optional Text) → Text
    , defaultMap : ∀(a : Type) → ∀(f : a → Text) → ∀(o : Optional a) → Text
    , lowerASCII : ∀(nil : Text) → Text
    , replace :
        ∀(needle : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text
    , replicate : ∀(num : Natural) → ∀(text : Text) → Text
    , shell-escape : ∀(xs : Text) → Text
    , show : Text → Text
    , spaces : ∀(a : Natural) → Text
    , upperASCII : ∀(nil : Text) → Text
    }
, XML :
    { Type : Type
    , attribute :
        ∀(key : Text) → ∀(value : Text) → { mapKey : Text, mapValue : Text }
    , element :
        ∀ ( elem
          : { attributes : List { mapKey : Text, mapValue : Text }
            , content :
                List
                  ( ∀(XML : Type) →
                    ∀ ( xml
                      : { element :
                            { attributes :
                                List { mapKey : Text, mapValue : Text }
                            , content : List XML
                            , name : Text
                            } →
                              XML
                        , rawText : Text → XML
                        , text : Text → XML
                        }
                      ) →
                      XML
                  )
            , name : Text
            }
          ) →
        ∀(XML : Type) →
        ∀ ( xml
          : { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List XML
                , name : Text
                } →
                  XML
            , rawText : Text → XML
            , text : Text → XML
            }
          ) →
          XML
    , emptyAttributes : List { mapKey : Text, mapValue : Text }
    , leaf :
        ∀ ( elem
          : { attributes : List { mapKey : Text, mapValue : Text }
            , name : Text
            }
          ) →
        ∀(XML : Type) →
        ∀ ( xml
          : { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List XML
                , name : Text
                } →
                  XML
            , rawText : Text → XML
            , text : Text → XML
            }
          ) →
          XML
    , rawText :
        ∀(d : Text) →
        ∀(XML : Type) →
        ∀ ( xml
          : { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List XML
                , name : Text
                } →
                  XML
            , rawText : Text → XML
            , text : Text → XML
            }
          ) →
          XML
    , render :
        ∀ ( x
          : ∀(XML : Type) →
            ∀ ( xml
              : { element :
                    { attributes : List { mapKey : Text, mapValue : Text }
                    , content : List XML
                    , name : Text
                    } →
                      XML
                , rawText : Text → XML
                , text : Text → XML
                }
              ) →
              XML
          ) →
          Text
    , text :
        ∀(d : Text) →
        ∀(XML : Type) →
        ∀ ( xml
          : { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List XML
                , name : Text
                } →
                  XML
            , rawText : Text → XML
            , text : Text → XML
            }
          ) →
          XML
    }
}
