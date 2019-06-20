  λ(JSON : Type)
→ λ(string : Text → JSON)
→ λ(number : Double → JSON)
→ λ(object : List { mapKey : Text, mapValue : JSON } → JSON)
→ λ(array : List JSON → JSON)
→ λ(bool : Bool → JSON)
→ λ(null : JSON)
→ object
  [ { mapKey = "foo", mapValue = null }
  , { mapKey = "bar", mapValue = array [ number 1.0, bool True ] }
  ]
