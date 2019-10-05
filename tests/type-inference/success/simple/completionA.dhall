let Example =
      { Type = { name : Text, id : Optional Natural }
      , default = { name = "", id = None Natural }
      }

let ExtraFields = Example ∧ { extra = {=} }

let RequiredFields =
        { Type = { name : Text, id : Optional Natural }
        , default = { id = None Natural }
        }

let WrongDefaultType =
        { Type = { name : Text, id : Optional Natural }
        , default = { id = True }
        }

in  { example0 = Example::{=}
    , example1 = Example::{ name = "John Doe" }
    , example2 = Example::{ id = Some 0 }
    , example3 = Example::{ name = "Mary Jane", id = Some 0 }
    , example4 = ExtraFields::{=}
    , example5 = RequiredFields::{ name = "Jane Smith" }
    , example6 = WrongDefaultType::{ name = "Bob Roberts", id = Some 0 }
    }
