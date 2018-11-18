{ foo = ([] : List Integer) # [1, 2, 3] # ([1, 2, 3] : List Integer)
, bar = [] : Optional Integer
, baz = [1] : Optional Integer
} : { foo : List Integer, bar : Optional Integer, baz : Optional Integer }
