{- This test verifies that leading tabs are correctly stripped

   Since one line is blank and the other line only has tabs, they are all
   stripped since they all count as leading whitespace for dedenting purposes.

   The end result is that the dedented string is equivalent to:

```
''

''
```

   ... which desugars to a single "\n".

   Carefully note that the two spaces before the starting quotes are not
   counted for dedenting purposes.
-}
  ''

		''
