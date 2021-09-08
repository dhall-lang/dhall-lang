{-|
Escape a Text value such that it can be used safely in shells.
The escaping is done by replacing all `'` with `'"'"'` and wraps that string in
single quotes.

This works for all POSIX-compliant shells and some other shells like csh.
-}
let shell-escape
    : Text -> Text
    = \(xs : Text) -> "'${Text/replace "'" "'\"'\"'" xs}'"

let example0 = assert : shell-escape "foo" === "'foo'"

let example1 = assert : shell-escape "foo'bar" === "'foo'\"'\"'bar'"

in  shell-escape
