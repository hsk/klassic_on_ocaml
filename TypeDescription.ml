type typeDescription =
  | IntType
  | ShortType
  | ByteType
  | LongType
  | FloatType
  | DoubleType
  | BooleanType
  | UnitType
  | DynamicType
  | UnknownType
  | ErrorType
  | FunctionType of typeDescription list * typeDescription

let rec show = function
  | IntType -> "Int"
  | ShortType -> "Short"
  | ByteType -> "Byte"
  | LongType -> "Long"
  | FloatType -> "Float"
  | DoubleType -> "Double"
  | BooleanType -> "Boolean"
  | UnitType -> "Unit"
  | DynamicType -> "*"
  | UnknownType -> "?"
  | ErrorType -> "!"
  | FunctionType(paramTypes, returnType) -> Printf.sprintf "(%s) => %s" (String.concat ", " (List.map show paramTypes)) (show returnType)
