  { foo = False && Natural/even (1 + 2 * 3) || True == False != True }
∧ { bar = [ "ABC" ++ "DEF" ] # [ "GHI" ] } ⫽ { baz = True }
: { foo : Bool, baz: Bool } ⩓ { bar : List Text }
