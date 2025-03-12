{
    _1 = (Some { a = 1 }) with ?.a = 2
  , _2 = (Some { a = { b = 3 } }) with ?.a.b = 4
  , _3 = (Some { a = { b = 3 } }) with ?.a = { b = 4 }
  , _4 = (Some { a = Some { b = 3 } }) with ?.a.? = { b = 4 }
  , _5 = (Some { a = Some { b = 3 } }) with ?.a.?.b = 4
  , _6 = (Some { a = None { b : Natural } }) with ?.a.?.b = 4
  , _7 = (Some { a = Some { b = 3 } }) with ?.a = None { b : Natural }
  , _8 = (Some { a = Some { b = 3 } }) with ? = { a = None { b : Natural } }
  , _9 = (None { x : Natural }) with ?.x = 123
}
