let Optional/fold =
        missing
          sha256:c5b9d72f6f62bdaa0e196ac1c742cc175cd67a717b880fb8aec1333a5a4132cf
      ? ./fold.dhall

let Optional/null =
        missing
          sha256:3871180b87ecaba8b53fffb2a8b52d3fce98098fab09a6f759358b9e8042eedc
      ? ./null.dhall

let equal
    : forall (a : Type) -> (a -> a -> Bool) -> Optional a -> Optional a -> Bool
    = \(a : Type) ->
      \(compare : a -> a -> Bool) ->
      \(ox : Optional a) ->
      \(oy : Optional a) ->
        Optional/fold
          a
          ox
          Bool
          (\(x : a) -> Optional/fold a oy Bool (compare x) False)
          (Optional/null a oy)

in  equal
