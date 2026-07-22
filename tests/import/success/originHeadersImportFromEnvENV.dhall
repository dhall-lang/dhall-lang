toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `localhost:18443` = toMap {
          `User-Agent` = env:USER_AGENT as Text
        }
      }
      ''
  , USER_AGENT = "Dhall (from env)"
  }
