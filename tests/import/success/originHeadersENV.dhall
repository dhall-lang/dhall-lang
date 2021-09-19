toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `httpbin.org:443` = toMap {
          `User-Agent` = "Dhall"
        }
      }
      ''
  }
