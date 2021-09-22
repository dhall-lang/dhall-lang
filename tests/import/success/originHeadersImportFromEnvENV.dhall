toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `httpbin.org:443` = toMap {
          `User-Agent` = env:USER_AGENT as Text
        }
      }
      ''
      , USER_AGENT = "Dhall (from env)"
  }
