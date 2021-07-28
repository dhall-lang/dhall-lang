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
          ( Type →
            { array : List _ → _@1
            , bool : Bool → _@1
            , double : Double → _@1
            , integer : Integer → _@1
            , null : _
            , object : List { mapKey : Text, mapValue : _ } → _@1
            , string : Text → _@1
            } →
              _@1
          ) →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , bool :
        Bool →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , double :
        Double →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , integer :
        Integer →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , keyText : Text → Text → { mapKey : Text, mapValue : Text }
    , keyValue : Type → Text → _@1 → { mapKey : Text, mapValue : _@2 }
    , natural :
        Natural →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , null :
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , number :
        Double →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , object :
        List
          { mapKey : Text
          , mapValue :
              Type →
              { array : List _ → _@1
              , bool : Bool → _@1
              , double : Double → _@1
              , integer : Integer → _@1
              , null : _
              , object : List { mapKey : Text, mapValue : _ } → _@1
              , string : Text → _@1
              } →
                _@1
          } →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , omitNullFields :
        ( Type →
          { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          } →
            _@1
        ) →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , render :
        ( Type →
          { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          } →
            _@1
        ) →
          Text
    , renderCompact :
        ( Type →
          { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          } →
            _@1
        ) →
          Text
    , renderInteger : Integer → Text
    , renderYAML :
        ( Type →
          { array : List _ → _@1
          , bool : Bool → _@1
          , double : Double → _@1
          , integer : Integer → _@1
          , null : _
          , object : List { mapKey : Text, mapValue : _ } → _@1
          , string : Text → _@1
          } →
            _@1
        ) →
          Text
    , string :
        Text →
        Type →
        { array : List _ → _@1
        , bool : Bool → _@1
        , double : Double → _@1
        , integer : Integer → _@1
        , null : _
        , object : List { mapKey : Text, mapValue : _ } → _@1
        , string : Text → _@1
        } →
          _@1
    , tagInline :
        Text →
        Type →
        _ →
          { contents : _@1, field : Text, nesting : < Inline | Nested : Text > }
    , tagNested :
        Text →
        Text →
        Type →
        _ →
          { contents : _@1, field : Text, nesting : < Inline | Nested : Text > }
    }
, List :
    { all : Type → (_ → Bool) → List _@1 → Bool
    , any : Type → (_ → Bool) → List _@1 → Bool
    , build : Type → (Type → (_@1 → _@1 → _@2) → _@1 → _@2) → List _@1
    , concat : Type → List (List _) → List _@1
    , concatMap : Type → Type → (_@1 → List _@1) → List _@2 → List _@2
    , default : Type → Optional (List _) → List _@1
    , drop : Natural → Type → List _ → List _@1
    , empty : Type → List _
    , filter : Type → (_ → Bool) → List _@1 → List _@2
    , fold : Type → List _ → Type → (_@2 → _@1 → _@2) → _@1 → _@2
    , foldLeft : Type → List _ → Type → (_ → _@3 → _@2) → _@1 → _@2
    , generate : Natural → Type → (Natural → _@1) → List _@1
    , head : Type → List _ → Optional _@1
    , index : Natural → Type → List _ → Optional _@1
    , indexed : Type → List _ → List { index : Natural, value : _@1 }
    , iterate : Natural → Type → (_ → _@1) → _@1 → List _@2
    , last : Type → List _ → Optional _@1
    , length : Type → List _ → Natural
    , map : Type → Type → (_@1 → _@1) → List _@2 → List _@2
    , null : Type → List _ → Bool
    , partition :
        Type → (_ → Bool) → List _@1 → { false : List _@2, true : List _@2 }
    , replicate : Natural → Type → _ → List _@1
    , reverse : Type → List _ → List _@1
    , shifted :
        Type →
        List (List { index : Natural, value : _ }) →
          List { index : Natural, value : _@1 }
    , take : Natural → Type → List _ → List _@1
    , unpackOptionals : Type → List (Optional _) → List _@1
    , unzip :
        Type →
        Type →
        List { _1 : _@1, _2 : _ } →
          { _1 : List _@2, _2 : List _@1 }
    , zip : Type → List _ → Type → List _ → List { _1 : _@3, _2 : _@1 }
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
        Type →
        Type →
        Type →
        (_@1 → _@1) →
        List { mapKey : _@3, mapValue : _@2 } →
          List { mapKey : _@4, mapValue : _@2 }
    , unpackOptionals :
        Type →
        Type →
        List { mapKey : _@1, mapValue : Optional _ } →
          List { mapKey : _@2, mapValue : _@1 }
    , values : Type → Type → List { mapKey : _@1, mapValue : _ } → List _@1
    }
, Monoid : Type → Type
, Natural :
    { build : (Type → (_ → _@1) → _@1 → _@2) → Natural
    , enumerate : Natural → List Natural
    , equal : Natural → Natural → Bool
    , even : Natural → Bool
    , fold : Natural → Type → (_ → _@1) → _@1 → _@2
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
, NonEmpty :
    { Type : Type → Type
    , all : Type → (_ → Bool) → { head : _@1, tail : List _@1 } → Bool
    , any : Type → (_ → Bool) → { head : _@1, tail : List _@1 } → Bool
    , concat :
        Type →
        { head : { head : _, tail : List _ }
        , tail : List { head : _, tail : List _ }
        } →
          { head : _@1, tail : List _@1 }
    , concatMap :
        Type →
        Type →
        (_@1 → { head : _@1, tail : List _@1 }) →
        { head : _@2, tail : List _@2 } →
          { head : _@2, tail : List _@2 }
    , head : Type → { head : _, tail : List _ } → _@1
    , index : Natural → Type → { head : _, tail : List _ } → Optional _@1
    , indexed :
        Type →
        { head : _, tail : List _ } →
          { head : { index : Natural, value : _@1 }
          , tail : List { index : Natural, value : _@1 }
          }
    , last : Type → { head : _, tail : List _ } → _@1
    , length : Type → { head : _, tail : List _ } → Natural
    , make : Type → _ → List _@1 → { head : _@2, tail : List _@2 }
    , map :
        Type →
        Type →
        (_@1 → _@1) →
        { head : _@2, tail : List _@2 } →
          { head : _@2, tail : List _@2 }
    , reverse :
        Type → { head : _, tail : List _ } → { head : _@1, tail : List _@1 }
    , shifted :
        Type →
        { head :
            { head : { index : Natural, value : _ }
            , tail : List { index : Natural, value : _ }
            }
        , tail :
            List
              { head : { index : Natural, value : _ }
              , tail : List { index : Natural, value : _ }
              }
        } →
          { head : { index : Natural, value : _@1 }
          , tail : List { index : Natural, value : _@1 }
          }
    , singleton : Type → _ → { head : _@1, tail : List _@1 }
    , toList : Type → { head : _, tail : List _ } → List _@1
    , unzip :
        Type →
        Type →
        { head : { _1 : _@1, _2 : _ }, tail : List { _1 : _@1, _2 : _ } } →
          { _1 : { head : _@2, tail : List _@2 }
          , _2 : { head : _@1, tail : List _@1 }
          }
    , zip :
        Type →
        { head : _, tail : List _ } →
        Type →
        { head : _, tail : List _ } →
          { head : { _1 : _@3, _2 : _@1 }, tail : List { _1 : _@3, _2 : _@1 } }
    }
, Operator :
    { `!=` : Bool → Bool → Bool
    , `#` : Type → List _ → List _@1 → List _@2
    , `&&` : Bool → Bool → Bool
    , `*` : Natural → Natural → Natural
    , `+` : Natural → Natural → Natural
    , `++` : Text → Text → Text
    , `==` : Bool → Bool → Bool
    , `||` : Bool → Bool → Bool
    }
, Optional :
    { all : Type → (_ → Bool) → Optional _@1 → Bool
    , any : Type → (_ → Bool) → Optional _@1 → Bool
    , build : Type → (Type → (_@1 → _@1) → _@1 → _@2) → Optional _@1
    , concat : Type → Optional (Optional _) → Optional _@1
    , concatMap :
        Type → Type → (_@1 → Optional _@1) → Optional _@2 → Optional _@2
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
        Type →
        Type →
        Optional { _1 : _@1, _2 : _ } →
          { _1 : Optional _@2, _2 : Optional _@1 }
    }
, Text :
    { concat : List Text → Text
    , concatMap : Type → (_ → Text) → List _@1 → Text
    , concatMapSep : Text → Type → (_ → Text) → List _@1 → Text
    , concatSep : Text → List Text → Text
    , default : Optional Text → Text
    , defaultMap : Type → (_ → Text) → Optional _@1 → Text
    , lowerASCII : Text → Text
    , replace : Text → Text → Text → Text
    , replicate : Natural → Text → Text
    , show : Text → Text
    , spaces : Natural → Text
    , upperASCII : Text → Text
    }
, XML :
    { Type : Type
    , attribute : Text → Text → { mapKey : Text, mapValue : Text }
    , element :
        { attributes : List { mapKey : Text, mapValue : Text }
        , content :
            List
              ( Type →
                { element :
                    { attributes : List { mapKey : Text, mapValue : Text }
                    , content : List _
                    , name : Text
                    } →
                      _@1
                , text : Text → _@1
                } →
                  _@1
              )
        , name : Text
        } →
        Type →
        { element :
            { attributes : List { mapKey : Text, mapValue : Text }
            , content : List _
            , name : Text
            } →
              _@1
        , text : Text → _@1
        } →
          _@1
    , emptyAttributes : List { mapKey : Text, mapValue : Text }
    , leaf :
        { attributes : List { mapKey : Text, mapValue : Text }, name : Text } →
        Type →
        { element :
            { attributes : List { mapKey : Text, mapValue : Text }
            , content : List _
            , name : Text
            } →
              _@1
        , text : Text → _@1
        } →
          _@1
    , render :
        ( Type →
          { element :
              { attributes : List { mapKey : Text, mapValue : Text }
              , content : List _
              , name : Text
              } →
                _@1
          , text : Text → _@1
          } →
            _@1
        ) →
          Text
    , text :
        Text →
        Type →
        { element :
            { attributes : List { mapKey : Text, mapValue : Text }
            , content : List _
            , name : Text
            } →
              _@1
        , text : Text → _@1
        } →
          _@1
    }
}
