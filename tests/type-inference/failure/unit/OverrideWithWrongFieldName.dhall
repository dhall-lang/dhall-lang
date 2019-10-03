let Example =
      { Type = { name : Text, id : Optional Natural }
      , default = { name = "", id = None Natural }
      }

in  Example::{ nam = "John Doe" }
