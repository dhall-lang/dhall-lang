toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `localhost:18443` = toMap {
          `User-Agent` = ./dhall-lang/tests/import/data/userAgent.dhall
        }
      }
      ''
  }
