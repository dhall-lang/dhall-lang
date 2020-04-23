{ Bool :
    { and : List Bool → Bool
    , build : (Type → _ → _@1 → _@2) → Bool
    , even : List Bool → Bool
    , fold : Bool → Type → _ → _@1 → _@2
    , not : Bool → Bool
    , odd : List Bool → Bool
    , or : List Bool → Bool
    , show : Bool → Text
    }
, Double : { show : Double → Text }
, Function :
    { compose : Type → Type → Type → (_@2 → _@2) → (_@2 → _@2) → _@4 → _@3
    , identity : Type → _ → _@1
    , sequence : Type → Type → List (_@1 → _@1) → _@2 → List _@2
    }
, Integer :
    { abs : Integer → Natural
    , add : Integer → Integer → Integer
    , clamp : Integer → Natural
    , equal : Integer → Integer → Bool
    , greaterThan : Integer → Integer → Bool
    , greaterThanEqual : Integer → Integer → Bool
    , lessThan : Integer → Integer → Bool
    , lessThanEqual : Integer → Integer → Bool
    , multiply : Integer → Integer → Integer
    , negate : Integer → Integer
    , negative : Integer → Bool
    , nonNegative : Integer → Bool
    , nonPositive : Integer → Bool
    , positive : Integer → Bool
    , show : Integer → Text
    , subtract : Integer → Integer → Integer
    , toDouble : Integer → Double
    , toNatural : Integer → Optional Natural
    }
, JSON :
    { Nesting : Type
    , Tagged : Type → Type
    , Type : Type
    , array :
          List
            (   Type
              → { array : List _ → _@1
                , bool : Bool → _@1
                , double : Double → _@1
                , integer : Integer → _@1
                , null : _
                , object : List { mapKey : Text, mapValue : _ } → _@1
                , string : Text → _@1
                }
              → _@1
            )
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , bool :
          Bool
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , double :
          Double
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , integer :
          Integer
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , keyText : Text → Text → { mapKey : Text, mapValue : Text }
    , keyValue : Type → Text → _@1 → { mapKey : Text, mapValue : _@2 }
    , natural :
          Natural
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , null :
          Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , number :
          Double
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , object :
          List
            { mapKey : Text
            , mapValue :
                  Type
                → { array : List _ → _@1
                  , bool : Bool → _@1
                  , double : Double → _@1
                  , integer : Integer → _@1
                  , null : _
                  , object : List { mapKey : Text, mapValue : _ } → _@1
                  , string : Text → _@1
                  }
                → _@1
            }
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , omitNullFields :
          (   Type
            → { array : List _ → _@1
              , bool : Bool → _@1
              , double : Double → _@1
              , integer : Integer → _@1
              , null : _
              , object : List { mapKey : Text, mapValue : _ } → _@1
              , string : Text → _@1
              }
            → _@1
          )
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , render :
          (   Type
            → { array : List _ → _@1
              , bool : Bool → _@1
              , double : Double → _@1
              , integer : Integer → _@1
              , null : _
              , object : List { mapKey : Text, mapValue : _ } → _@1
              , string : Text → _@1
              }
            → _@1
          )
        → Text
    , renderInteger : Integer → Text
    , renderYAML :
          (   Type
            → { array : List _ → _@1
              , bool : Bool → _@1
              , double : Double → _@1
              , integer : Integer → _@1
              , null : _
              , object : List { mapKey : Text, mapValue : _ } → _@1
              , string : Text → _@1
              }
            → _@1
          )
        → Text
    , string :
          Text
        → Type
        → { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          }
        → _@1
    , tagInline :
          Text
        → Type
        → _
        → { contents : _@1, field : Text, nesting : < Inline | Nested : Text > }
    , tagNested :
          Text
        → Text
        → Type
        → _
        → { contents : _@1, field : Text, nesting : < Inline | Nested : Text > }
    }
, List :
    { all : Type → (_ → Bool) → List _@1 → Bool
    , any : Type → (_ → Bool) → List _@1 → Bool
    , build :
          ∀(a : Type)
        → (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list)
        → List a
    , concat : Type → List (List _) → List _@1
    , concatMap : Type → Type → (_@1 → List _@1) → List _@2 → List _@2
    , default : Type → Optional (List _) → List _@1
    , drop : Natural → Type → List _ → List _@1
    , empty : Type → List _
    , filter : Type → (_ → Bool) → List _@1 → List _@2
    , fold :
          ∀(a : Type)
        → List a
        → ∀(list : Type)
        → ∀(cons : a → list → list)
        → ∀(nil : list)
        → list
    , generate : Natural → Type → (Natural → _@1) → List _@1
    , head : ∀(a : Type) → List a → Optional a
    , indexed : ∀(a : Type) → List a → List { index : Natural, value : a }
    , iterate : Natural → Type → (_ → _@1) → _@1 → List _@2
    , last : ∀(a : Type) → List a → Optional a
    , length : ∀(a : Type) → List a → Natural
    , map : Type → Type → (_@1 → _@1) → List _@2 → List _@2
    , null : Type → List _ → Bool
    , partition :
        Type → (_ → Bool) → List _@1 → { false : List _@2, true : List _@2 }
    , replicate : Natural → Type → _ → List _@1
    , reverse : ∀(a : Type) → List a → List a
    , shifted :
          Type
        → List (List { index : Natural, value : _ })
        → List { index : Natural, value : _@1 }
    , take : Natural → Type → List _ → List _@1
    , unzip :
          Type
        → Type
        → List { _1 : _@1, _2 : _ }
        → { _1 : List _@2, _2 : List _@1 }
    }
, Location : { Type : Type }
, Map :
    { Entry : Type → Type → Type
    , Type : Type → Type → Type
    , empty : Type → Type → List { mapKey : _@1, mapValue : _ }
    , keyText : Text → Text → { mapKey : Text, mapValue : Text }
    , keyValue : Type → Text → _@1 → { mapKey : Text, mapValue : _@2 }
    , keys : Type → Type → List { mapKey : _@1, mapValue : _ } → List _@2
    , map :
          Type
        → Type
        → Type
        → (_@1 → _@1)
        → List { mapKey : _@3, mapValue : _@2 }
        → List { mapKey : _@4, mapValue : _@2 }
    , values : Type → Type → List { mapKey : _@1, mapValue : _ } → List _@1
    }
, Monoid : Type → Type
, Natural :
    { build :
          (   ∀(natural : Type)
            → ∀(succ : natural → natural)
            → ∀(zero : natural)
            → natural
          )
        → Natural
    , enumerate : Natural → List Natural
    , equal : Natural → Natural → Bool
    , even : Natural → Bool
    , fold :
          Natural
        → ∀(natural : Type)
        → ∀(succ : natural → natural)
        → ∀(zero : natural)
        → natural
    , greaterThan : Natural → Natural → Bool
    , greaterThanEqual : Natural → Natural → Bool
    , isZero : Natural → Bool
    , lessThan : Natural → Natural → Bool
    , lessThanEqual : Natural → Natural → Bool
    , listMax : List Natural → Optional Natural
    , listMin : List Natural → Optional Natural
    , max : Natural → Natural → Natural
    , min : Natural → Natural → Natural
    , odd : Natural → Bool
    , product : List Natural → Natural
    , show : Natural → Text
    , sort : List Natural → List Natural
    , subtract : Natural → Natural → Natural
    , sum : List Natural → Natural
    , toDouble : Natural → Double
    , toInteger : Natural → Integer
    }
, Optional :
    { all : Type → (_ → Bool) → Optional _@1 → Bool
    , any : Type → (_ → Bool) → Optional _@1 → Bool
    , build : Type → (Type → (_@1 → _@1) → _@1 → _@2) → Optional _@1
    , concat : Type → Optional (Optional _) → Optional _@1
    , default : Type → _ → Optional _@1 → _@2
    , filter : Type → (_ → Bool) → Optional _@1 → Optional _@2
    , fold : Type → Optional _ → Type → (_@2 → _@1) → _@1 → _@2
    , head : Type → List (Optional _) → Optional _@1
    , last : Type → List (Optional _) → Optional _@1
    , length : Type → Optional _ → Natural
    , map : Type → Type → (_@1 → _@1) → Optional _@2 → Optional _@2
    , null : Type → Optional _ → Bool
    , toList : Type → Optional _ → List _@1
    , unzip :
          Type
        → Type
        → Optional { _1 : _@1, _2 : _ }
        → { _1 : Optional _@2, _2 : Optional _@1 }
    }
, Text :
    { concat : List Text → Text
    , concatMap : Type → (_ → Text) → List _@1 → Text
    , concatMapSep : Text → Type → (_ → Text) → List _@1 → Text
    , concatSep : Text → List Text → Text
    , default : Optional Text → Text
    , defaultMap : Type → (_ → Text) → Optional _@1 → Text
    , show : Text → Text
    }
, XML :
    { Type : Type
    , attribute : Text → Text → { mapKey : Text, mapValue : Text }
    , element :
          { attributes : List { mapKey : Text, mapValue : Text }
          , content :
              List
                (   Type
                  → { element :
                          { attributes : List { mapKey : Text, mapValue : Text }
                          , content : List _
                          , name : Text
                          }
                        → _@1
                    , text : Text → _@1
                    }
                  → _@1
                )
          , name : Text
          }
        → Type
        → { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List _
                , name : Text
                }
              → _@1
          , text : Text → _@1
          }
        → _@1
    , emptyAttributes : List { mapKey : Text, mapValue : Text }
    , leaf :
          { attributes : List { mapKey : Text, mapValue : Text }, name : Text }
        → Type
        → { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List _
                , name : Text
                }
              → _@1
          , text : Text → _@1
          }
        → _@1
    , render :
          (   Type
            → { element :
                    { attributes : List { mapKey : Text, mapValue : Text }
                    , content : List _
                    , name : Text
                    }
                  → _@1
              , text : Text → _@1
              }
            → _@1
          )
        → Text
    , text :
          Text
        → Type
        → { element :
                { attributes : List { mapKey : Text, mapValue : Text }
                , content : List _
                , name : Text
                }
              → _@1
          , text : Text → _@1
          }
        → _@1
    }
}
