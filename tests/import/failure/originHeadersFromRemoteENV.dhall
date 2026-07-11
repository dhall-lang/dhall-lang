-- Use of a remote import causes Cyclic Import on env:DHALL_HEADERS
toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `localhost:18080` = toMap {
          `User-Agent` = http://example.com as Text
        }
      }
      ''
  }
