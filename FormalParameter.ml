open TypeDescription

type formalParameter =
  | FormalParameter of string * typeDescription

let formalParameter s = FormalParameter(s, DynamicType)
