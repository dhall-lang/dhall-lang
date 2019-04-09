{ foo = ([] : List Natural) # [1, 2, 3] # ([1, 2, 3] : List Natural)
, bar = [] : Optional Natural
, baz = [1] : Optional Natural
} : { foo : List Natural, bar : Optional Natural, baz : Optional Natural }
