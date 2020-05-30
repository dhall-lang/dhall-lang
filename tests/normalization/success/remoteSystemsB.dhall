  λ ( xs
    : List
      { cores :
          Natural
      , host :
          Text
      , key :
          Text
      , mandatoryFeatures :
          List Text
      , platforms :
          List Text
      , speedFactor :
          Natural
      , supportedFeatures :
          List Text
      , user :
          Optional Text
      }
    )
→ List/fold
  { cores :
      Natural
  , host :
      Text
  , key :
      Text
  , mandatoryFeatures :
      List Text
  , platforms :
      List Text
  , speedFactor :
      Natural
  , supportedFeatures :
      List Text
  , user :
      Optional Text
  }
  xs
  Text
  (   λ ( x
        : { cores :
              Natural
          , host :
              Text
          , key :
              Text
          , mandatoryFeatures :
              List Text
          , platforms :
              List Text
          , speedFactor :
              Natural
          , supportedFeatures :
              List Text
          , user :
              Optional Text
          }
        )
    → λ(y : Text)
    →     ''
          ${merge
            { None = x.host, Some = λ(user : Text) → "${user}@${x.host}" }
            x.user} ${merge
                      { Empty = "", NonEmpty = λ(result : Text) → result }
                      ( List/fold
                        Text
                        x.platforms
                        < Empty | NonEmpty : Text >
                        (   λ(element : Text)
                          → λ(status : < Empty | NonEmpty : Text >)
                          → merge
                            { Empty =
                                < Empty | NonEmpty : Text >.NonEmpty element
                            , NonEmpty =
                                  λ(result : Text)
                                → < Empty | NonEmpty : Text >.NonEmpty
                                  ("${element},${result}")
                            }
                            status
                        )
                        < Empty | NonEmpty : Text >.Empty
                      )} ${x.key} ${Integer/show
                                    ( Natural/toInteger x.cores
                                    )} ${Integer/show
                                         ( Natural/toInteger x.speedFactor
                                         )} ${merge
                                              { Empty =
                                                  ""
                                              , NonEmpty =
                                                  λ(result : Text) → result
                                              }
                                              ( List/fold
                                                Text
                                                x.supportedFeatures
                                                < Empty | NonEmpty : Text >
                                                (   λ(element : Text)
                                                  → λ ( status
                                                      : < Empty
                                                        | NonEmpty :
                                                            Text
                                                        >
                                                      )
                                                  → merge
                                                    { Empty =
                                                        < Empty
                                                        | NonEmpty :
                                                            Text
                                                        >.NonEmpty
                                                        element
                                                    , NonEmpty =
                                                          λ(result : Text)
                                                        → < Empty
                                                          | NonEmpty :
                                                              Text
                                                          >.NonEmpty
                                                          "${element},${result}"
                                                    }
                                                    status
                                                )
                                                < Empty
                                                | NonEmpty :
                                                    Text
                                                >.Empty
                                              )} ${merge
                                                   { Empty =
                                                       ""
                                                   , NonEmpty =
                                                       λ(result : Text) → result
                                                   }
                                                   ( List/fold
                                                     Text
                                                     x.mandatoryFeatures
                                                     < Empty | NonEmpty : Text >
                                                     (   λ(element : Text)
                                                       → λ ( status
                                                           : < Empty
                                                             | NonEmpty :
                                                                 Text
                                                             >
                                                           )
                                                       → merge
                                                         { Empty =
                                                             < Empty
                                                             | NonEmpty :
                                                                 Text
                                                             >.NonEmpty
                                                             element
                                                         , NonEmpty =
                                                               λ(result : Text)
                                                             → < Empty
                                                               | NonEmpty :
                                                                   Text
                                                               >.NonEmpty
                                                               "${element},${result}"
                                                         }
                                                         status
                                                     )
                                                     < Empty
                                                     | NonEmpty :
                                                         Text
                                                     >.Empty
                                                   )}
          ${y}''
  )
  ""
