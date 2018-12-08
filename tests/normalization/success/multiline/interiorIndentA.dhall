{- The indent is computed as the minimum number of leading spaces over all lines
   in a multi-line literal, including the line right before the closing quotes.
   In this case, there are three lines:

   * The first line containing "  foo" with two leading spaces
   * The second line containing "  bar" with two leading spaces
   * The third line containing "" with zero leading spaces

   Since the last line has zero leading spaces, the minimum number of leading
   spaces over all three lines is zero, so we don't strip any spaces.
-}

''
  foo
  bar
''
