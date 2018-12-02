-- Verify that users can use `.` to both access a record field and a union
-- constructor within the same expression.  This is a common idiom if a user
-- provides a types package.

let Scope = < Public : {} | Private : {} >
let types = { Scope = Scope }
in types.Scope.Public {=}
