toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `localhost:18080` = toMap {
          `User-Agent` = "Dhall"
        }
      }
      ''
  }
