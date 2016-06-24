type operator =
  | ADD
  | SUBTRACT
  | DIVIDE
  | MULTIPLY
  | EQUAL
  | LESS_THAN
  | LESS_OR_EQUAL
  | GREATER_THAN
  | GREATER_EQUAL
  | AND2
  | BAR2

let show = function
  | ADD -> "+"
  | SUBTRACT -> "-"
  | DIVIDE -> "/"
  | MULTIPLY -> "*"
  | EQUAL -> "=="
  | LESS_THAN -> "<"
  | LESS_OR_EQUAL -> "<="
  | GREATER_THAN -> ">"
  | GREATER_EQUAL -> ">="
  | AND2 -> "&&"
  | BAR2 -> "||"

