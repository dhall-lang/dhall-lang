    let Text/concatMap =
(          ../../Prelude/package.dhall).`Text`.concatMap 

in  let Text/concatSep =
(          ../../Prelude/package.dhall).`Text`.concatSep 

in  let Row =
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

in  let renderRow =
            λ ( row
              : Row
              )
          →     let host =
                      Optional/fold
                      Text
                      row.user
                      Text
                      (λ(user : Text) → "${user}@${row.host}")
                      row.host
            
            in  let platforms = Text/concatSep "," row.platforms
            
            in  let key = row.key
            
            in  let cores = Integer/show (Natural/toInteger row.cores)
            
            in  let speedFactor =
                      Integer/show (Natural/toInteger row.speedFactor)
            
            in  let supportedFeatures = Text/concatSep "," row.supportedFeatures
            
            in  let mandatoryFeatures = Text/concatSep "," row.mandatoryFeatures
            
            in  ''
                ${host} ${platforms} ${key} ${cores} ${speedFactor} ${supportedFeatures} ${mandatoryFeatures}
                ''

in  Text/concatMap Row renderRow
