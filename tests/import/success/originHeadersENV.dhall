toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `localhost:18443` = toMap {
          `User-Agent` = "Dhall"
        }
      }
      ''
  }
