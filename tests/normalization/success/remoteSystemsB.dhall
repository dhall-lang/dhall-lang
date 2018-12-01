  λ ( _
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
  _
  Text
  (   λ ( _
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
    → λ(_ : Text)
    →     ''
          ${Optional/fold
            Text
            _@1.user
            Text
            (λ(user : Text) → "${user}@${_@1.host}")
            _@1.host} ${merge
                        { Empty = λ(_ : {}) → "", NonEmpty = λ(_ : Text) → _ }
                        ( List/fold
                          Text
                          _@1.platforms
                          < Empty : {} | NonEmpty : Text >
                          (   λ(_ : Text)
                            → λ(_ : < Empty : {} | NonEmpty : Text >)
                            → merge
                              { Empty =
                                  λ(_ : {}) → < NonEmpty = _@2 | Empty : {} >
                              , NonEmpty =
                                    λ(_ : Text)
                                  → < NonEmpty = _@2 ++ "," ++ _ | Empty : {} >
                              }
                              _
                              : < Empty : {} | NonEmpty : Text >
                          )
                          < Empty = {=} | NonEmpty : Text >
                        )
                        : Text} ${_@1.key} ${Integer/show
                                             ( Natural/toInteger _@1.cores
                                             )} ${Integer/show
                                                  ( Natural/toInteger
                                                    _@1.speedFactor
                                                  )} ${merge
                                                       { Empty =
                                                           λ(_ : {}) → ""
                                                       , NonEmpty =
                                                           λ(_ : Text) → _
                                                       }
                                                       ( List/fold
                                                         Text
                                                         _@1.supportedFeatures
                                                         < Empty :
                                                             {}
                                                         | NonEmpty :
                                                             Text
                                                         >
                                                         (   λ(_ : Text)
                                                           → λ ( _
                                                               : < Empty :
                                                                     {}
                                                                 | NonEmpty :
                                                                     Text
                                                                 >
                                                               )
                                                           → merge
                                                             { Empty =
                                                                   λ(_ : {})
                                                                 → < NonEmpty =
                                                                       _@2
                                                                   | Empty :
                                                                       {}
                                                                   >
                                                             , NonEmpty =
                                                                   λ(_ : Text)
                                                                 → < NonEmpty =
                                                                           _@2
                                                                       ++  ","
                                                                       ++  _
                                                                   | Empty :
                                                                       {}
                                                                   >
                                                             }
                                                             _
                                                             : < Empty :
                                                                   {}
                                                               | NonEmpty :
                                                                   Text
                                                               >
                                                         )
                                                         < Empty =
                                                             {=}
                                                         | NonEmpty :
                                                             Text
                                                         >
                                                       )
                                                       : Text} ${merge
                                                                 { Empty =
                                                                       λ(_ : {})
                                                                     → ""
                                                                 , NonEmpty =
                                                                       λ ( _
                                                                         : Text
                                                                         )
                                                                     → _
                                                                 }
                                                                 ( List/fold
                                                                   Text
                                                                   _@1.mandatoryFeatures
                                                                   < Empty :
                                                                       {}
                                                                   | NonEmpty :
                                                                       Text
                                                                   >
                                                                   (   λ ( _
                                                                         : Text
                                                                         )
                                                                     → λ ( _
                                                                         : < Empty :
                                                                               {}
                                                                           | NonEmpty :
                                                                               Text
                                                                           >
                                                                         )
                                                                     → merge
                                                                       { Empty =
                                                                             λ ( _
                                                                               : {}
                                                                               )
                                                                           → < NonEmpty =
                                                                                 _@2
                                                                             | Empty :
                                                                                 {}
                                                                             >
                                                                       , NonEmpty =
                                                                             λ ( _
                                                                               : Text
                                                                               )
                                                                           → < NonEmpty =
                                                                                     _@2
                                                                                 ++  ","
                                                                                 ++  _
                                                                             | Empty :
                                                                                 {}
                                                                             >
                                                                       }
                                                                       _
                                                                       : < Empty :
                                                                             {}
                                                                         | NonEmpty :
                                                                             Text
                                                                         >
                                                                   )
                                                                   < Empty =
                                                                       {=}
                                                                   | NonEmpty :
                                                                       Text
                                                                   >
                                                                 )
                                                                 : Text}
          ''
      ++  _
  )
  ""
