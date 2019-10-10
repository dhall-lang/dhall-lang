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
          ∀(A : Type)
        → ∀(B : Type)
        → ∀(C : Type)
        → ∀(f : A → B)
        → ∀(g : B → C)
        → ∀(x : A)
        → C
    }
, Integer : { show : Integer → Text, toDouble : Integer → Double }
, JSON :
    { Nesting : Type
    , Tagged : ∀(a : Type) → Type
    , Type : Type
    , array :
          ∀ ( x
            : List
                (   ∀(JSON : Type)
                  → ∀ ( json
                      : { array : List JSON → JSON
                        , bool : Bool → JSON
                        , null : JSON
                        , number : Double → JSON
                        , object :
                            List { mapKey : Text, mapValue : JSON } → JSON
                        , string : Text → JSON
                        }
                      )
                  → JSON
                )
            )
        → ∀(JSON : Type)
        → ∀ ( json
            : { array : List JSON → JSON
              , bool : Bool → JSON
              , null : JSON
              , number : Double → JSON
              , object : List { mapKey : Text, mapValue : JSON } → JSON
              , string : Text → JSON
              }
            )
        → JSON
    , bool :
          ∀(x : Bool)
        → ∀(JSON : Type)
        → ∀ ( json
            : { array : List JSON → JSON
              , bool : Bool → JSON
              , null : JSON
              , number : Double → JSON
              , object : List { mapKey : Text, mapValue : JSON } → JSON
              , string : Text → JSON
              }
            )
        → JSON
    , keyText :
        ∀(key : Text) → ∀(value : Text) → { mapKey : Text, mapValue : Text }
    , keyValue :
          ∀(v : Type)
        → ∀(key : Text)
        → ∀(value : v)
        → { mapKey : Text, mapValue : v }
    , null :
          ∀(JSON : Type)
        → ∀ ( json
            : { array : List JSON → JSON
              , bool : Bool → JSON
              , null : JSON
              , number : Double → JSON
              , object : List { mapKey : Text, mapValue : JSON } → JSON
              , string : Text → JSON
              }
            )
        → JSON
    , number :
          ∀(x : Double)
        → ∀(JSON : Type)
        → ∀ ( json
            : { array : List JSON → JSON
              , bool : Bool → JSON
              , null : JSON
              , number : Double → JSON
              , object : List { mapKey : Text, mapValue : JSON } → JSON
              , string : Text → JSON
              }
            )
        → JSON
    , object :
          ∀ ( x
            : List
                { mapKey : Text
                , mapValue :
                      ∀(JSON : Type)
                    → ∀ ( json
                        : { array : List JSON → JSON
                          , bool : Bool → JSON
                          , null : JSON
                          , number : Double → JSON
                          , object :
                              List { mapKey : Text, mapValue : JSON } → JSON
                          , string : Text → JSON
                          }
                        )
                    → JSON
                }
            )
        → ∀(JSON : Type)
        → ∀ ( json
            : { array : List JSON → JSON
              , bool : Bool → JSON
              , null : JSON
              , number : Double → JSON
              , object : List { mapKey : Text, mapValue : JSON } → JSON
              , string : Text → JSON
              }
            )
        → JSON
    , render :
          ∀ ( j
            :   ∀(JSON : Type)
              → ∀ ( json
                  : { array : List JSON → JSON
                    , bool : Bool → JSON
                    , null : JSON
                    , number : Double → JSON
                    , object : List { mapKey : Text, mapValue : JSON } → JSON
                    , string : Text → JSON
                    }
                  )
              → JSON
            )
        → Text
    , string :
          ∀(x : Text)
        → ∀(JSON : Type)
        → ∀ ( json
            : { array : List JSON → JSON
              , bool : Bool → JSON
              , null : JSON
              , number : Double → JSON
              , object : List { mapKey : Text, mapValue : JSON } → JSON
              , string : Text → JSON
              }
            )
        → JSON
    }
, List :
    { all : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : List a) → Bool
    , any : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : List a) → Bool
    , build :
          ∀(a : Type)
        → (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list)
        → List a
    , concat : ∀(a : Type) → ∀(xss : List (List a)) → List a
    , concatMap :
        ∀(a : Type) → ∀(b : Type) → ∀(f : a → List b) → ∀(xs : List a) → List b
    , default : ∀(a : Type) → ∀(o : Optional (List a)) → List a
    , empty : ∀(a : Type) → List a
    , filter : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : List a) → List a
    , fold :
          ∀(a : Type)
        → List a
        → ∀(list : Type)
        → ∀(cons : a → list → list)
        → ∀(nil : list)
        → list
    , generate : ∀(n : Natural) → ∀(a : Type) → ∀(f : Natural → a) → List a
    , head : ∀(a : Type) → List a → Optional a
    , indexed : ∀(a : Type) → List a → List { index : Natural, value : a }
    , iterate : ∀(n : Natural) → ∀(a : Type) → ∀(f : a → a) → ∀(x : a) → List a
    , last : ∀(a : Type) → List a → Optional a
    , length : ∀(a : Type) → List a → Natural
    , map : ∀(a : Type) → ∀(b : Type) → ∀(f : a → b) → ∀(xs : List a) → List b
    , null : ∀(a : Type) → ∀(xs : List a) → Bool
    , replicate : ∀(n : Natural) → ∀(a : Type) → ∀(x : a) → List a
    , reverse : ∀(a : Type) → List a → List a
    , shifted :
          ∀(a : Type)
        → ∀(kvss : List (List { index : Natural, value : a }))
        → List { index : Natural, value : a }
    , unzip :
          ∀(a : Type)
        → ∀(b : Type)
        → ∀(xs : List { _1 : a, _2 : b })
        → { _1 : List a, _2 : List b }
    }
, Location : { Type : Type }
, Map :
    { Entry : ∀(k : Type) → ∀(v : Type) → Type
    , Type : ∀(k : Type) → ∀(v : Type) → Type
    , empty : ∀(k : Type) → ∀(v : Type) → List { mapKey : k, mapValue : v }
    , keyText :
        ∀(key : Text) → ∀(value : Text) → { mapKey : Text, mapValue : Text }
    , keyValue :
          ∀(v : Type)
        → ∀(key : Text)
        → ∀(value : v)
        → { mapKey : Text, mapValue : v }
    , keys :
          ∀(k : Type)
        → ∀(v : Type)
        → ∀(xs : List { mapKey : k, mapValue : v })
        → List k
    , map :
          ∀(k : Type)
        → ∀(a : Type)
        → ∀(b : Type)
        → ∀(f : a → b)
        → ∀(m : List { mapKey : k, mapValue : a })
        → List { mapKey : k, mapValue : b }
    , values :
          ∀(k : Type)
        → ∀(v : Type)
        → ∀(xs : List { mapKey : k, mapValue : v })
        → List v
    }
, Monoid : ∀(m : Type) → Type
, Natural :
    { build :
          (   ∀(natural : Type)
            → ∀(succ : natural → natural)
            → ∀(zero : natural)
            → natural
          )
        → Natural
    , enumerate : ∀(n : Natural) → List Natural
    , equal : ∀(a : Natural) → ∀(b : Natural) → Bool
    , even : Natural → Bool
    , fold :
          Natural
        → ∀(natural : Type)
        → ∀(succ : natural → natural)
        → ∀(zero : natural)
        → natural
    , greaterThan : ∀(x : Natural) → ∀(y : Natural) → Bool
    , greaterThanEqual : ∀(x : Natural) → ∀(y : Natural) → Bool
    , isZero : Natural → Bool
    , lessThan : ∀(x : Natural) → ∀(y : Natural) → Bool
    , lessThanEqual : ∀(x : Natural) → ∀(y : Natural) → Bool
    , odd : Natural → Bool
    , product : ∀(xs : List Natural) → Natural
    , show : Natural → Text
    , subtract : Natural → Natural → Natural
    , sum : ∀(xs : List Natural) → Natural
    , toDouble : ∀(n : Natural) → Double
    , toInteger : Natural → Integer
    }
, Optional :
    { all : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : Optional a) → Bool
    , any : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : Optional a) → Bool
    , build :
          ∀(a : Type)
        → (   ∀(optional : Type)
            → ∀(just : a → optional)
            → ∀(nothing : optional)
            → optional
          )
        → Optional a
    , concat : ∀(a : Type) → ∀(x : Optional (Optional a)) → Optional a
    , default : ∀(a : Type) → ∀(default : a) → ∀(o : Optional a) → a
    , filter : ∀(a : Type) → ∀(f : a → Bool) → ∀(xs : Optional a) → Optional a
    , fold :
          ∀(a : Type)
        → Optional a
        → ∀(optional : Type)
        → ∀(just : a → optional)
        → ∀(nothing : optional)
        → optional
    , head : ∀(a : Type) → ∀(xs : List (Optional a)) → Optional a
    , last : ∀(a : Type) → ∀(xs : List (Optional a)) → Optional a
    , length : ∀(a : Type) → ∀(xs : Optional a) → Natural
    , map :
          ∀(a : Type)
        → ∀(b : Type)
        → ∀(f : a → b)
        → ∀(o : Optional a)
        → Optional b
    , null : ∀(a : Type) → ∀(xs : Optional a) → Bool
    , toList : ∀(a : Type) → ∀(o : Optional a) → List a
    , unzip :
          ∀(a : Type)
        → ∀(b : Type)
        → ∀(xs : Optional { _1 : a, _2 : b })
        → { _1 : Optional a, _2 : Optional b }
    }
, Text :
    { concat : ∀(xs : List Text) → Text
    , concatMap : ∀(a : Type) → ∀(f : a → Text) → ∀(xs : List a) → Text
    , concatMapSep :
          ∀(separator : Text)
        → ∀(a : Type)
        → ∀(f : a → Text)
        → ∀(elements : List a)
        → Text
    , concatSep : ∀(separator : Text) → ∀(elements : List Text) → Text
    , default : ∀(o : Optional Text) → Text
    , defaultMap : ∀(a : Type) → ∀(f : a → Text) → ∀(o : Optional a) → Text
    , show : Text → Text
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
                    (   ∀(XML : Type)
                      → ∀ ( xml
                          : { element :
                                  { attributes :
                                      List { mapKey : Text, mapValue : Text }
                                  , content : List XML
                                  , name : Text
                                  }
                                → XML
                            , text : Text → XML
                            }
                          )
                      → XML
                    )
              , name : Text
              }
            )
        → ∀(XML : Type)
        → ∀ ( xml
            : { element :
                    { attributes : List { mapKey : Text, mapValue : Text }
                    , content : List XML
                    , name : Text
                    }
                  → XML
              , text : Text → XML
              }
            )
        → XML
    , emptyAttributes : List { mapKey : Text, mapValue : Text }
    , leaf :
          ∀ ( elem
            : { attributes : List { mapKey : Text, mapValue : Text }
              , name : Text
              }
            )
        → ∀(XML : Type)
        → ∀ ( xml
            : { element :
                    { attributes : List { mapKey : Text, mapValue : Text }
                    , content : List XML
                    , name : Text
                    }
                  → XML
              , text : Text → XML
              }
            )
        → XML
    , render :
          ∀ ( x
            :   ∀(XML : Type)
              → ∀ ( xml
                  : { element :
                          { attributes : List { mapKey : Text, mapValue : Text }
                          , content : List XML
                          , name : Text
                          }
                        → XML
                    , text : Text → XML
                    }
                  )
              → XML
            )
        → Text
    , text :
          ∀(d : Text)
        → ∀(XML : Type)
        → ∀ ( xml
            : { element :
                    { attributes : List { mapKey : Text, mapValue : Text }
                    , content : List XML
                    , name : Text
                    }
                  → XML
              , text : Text → XML
              }
            )
        → XML
    }
}
