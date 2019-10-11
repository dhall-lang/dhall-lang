let Example =
      { Type = { name : Text, id : Optional Natural }
      , default = { name = "", id = None Natural }
      }

in  { example0 = Example::{=}
    , example1 = Example::{ name = "John Doe" }
    , example2 = Example::{ id = Some 0 }
    , example3 = Example::{ name = "Mary Jane", id = Some 0 }
    }
