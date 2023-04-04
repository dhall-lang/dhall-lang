--| This is the union type returned when you import something `as Location`
let Location
    : Type
    = < Environment : Text | Local : Text | Missing | Remote : Text >

let example0 =
        assert
      :   missing as Location
        ≡ < Environment : Text
          | Local : Text
          | Missing
          | Remote : Text
          >.Missing

in  Location
