{- This example verifies that an implementation is correctly breaking leading
   spaces at interpolated expressions.  The first line has the fewest number of
   leading spaces (2) due to the interruption by the interpolated expression.
-}

''
${Natural/show 1}      foo
  bar
''
