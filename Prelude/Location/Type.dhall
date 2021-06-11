--| This is the union type returned when you import something `as Location`
let Location
    : Type
    = < Environment : Text | Local : Text | Missing | Remote : Text >

let example0 =
        assert
      :     missing
              sha256:f428188ff9d77ea15bc2bcd0da3f8ed81b304e175b07ade42a3b0fb02941b2aa as Location
          ? missing as Location
        ≡ < Environment : Text
          | Local : Text
          | Missing
          | Remote : Text
          >.Missing

in  Location
