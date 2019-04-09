-- This test verifies that an implementation rejects the following expression at
-- parse time (i.e. due to an incomplete `if` expression) rather than at
-- type-checking time (i.e. due to an unbound variable named `if` or `else`)
--
-- This ensures that implementations are not treating keywords as potential
-- identifier names.  In other words, if they fail to parse an incomplete
-- `if` expression they don't try to alternatively parse the expression as
-- function application instead.

if a then b else
