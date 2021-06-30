{ and =
    \(_ : List Bool) ->
      List/fold Bool _ Bool (\(_ : Bool) -> \(_ : Bool) -> _@1 && _) True
, build = \(_ : Type -> _ -> _@1 -> _@2) -> _ Bool True False
, even =
    \(_ : List Bool) ->
      List/fold Bool _ Bool (\(_ : Bool) -> \(_ : Bool) -> _@1 == _) True
, fold =
    \(_ : Bool) ->
    \(_ : Type) ->
    \(_ : _) ->
    \(_ : _@1) ->
      if _@3 then _@1 else _
, not = \(_ : Bool) -> _ == False
, odd =
    \(_ : List Bool) ->
      List/fold Bool _ Bool (\(_ : Bool) -> \(_ : Bool) -> _@1 != _) False
, or =
    \(_ : List Bool) ->
      List/fold Bool _ Bool (\(_ : Bool) -> \(_ : Bool) -> _@1 || _) False
, show = \(_ : Bool) -> if _ then "True" else "False"
}
