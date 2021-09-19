-- Use of a remote import causes Cyclic Import on env:DHALL_HEADERS
toMap
  { DHALL_HEADERS =
      ''
      toMap {
        `httpbin.org:443` = toMap {
          `User-Agent` = http://example.com as Text
        }
      }
      ''
  }
