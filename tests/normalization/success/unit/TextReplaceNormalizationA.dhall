-- This test verifies that implementations do not perform Unicode normalization
-- before substitution.
Text/replace "a\u{0301}" "b" "\u{00e1}c"
