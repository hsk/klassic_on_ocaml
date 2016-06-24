open Operator
open TypeDescription
open FormalParameter

type location =
  | SourceLocation of int * int
  | NoLocation

let show_location = function
  | SourceLocation(line, column) -> Printf.sprintf "%d, %d :" line column
  | NoLocation                   -> "? :"

type import =
  | Import of location * string * string

type double = float
type short = int
type long = int
type byte = int

let string_of_short = string_of_int
let string_of_long = string_of_int
let string_of_byte = string_of_int
let string_of_double = string_of_float

type astNode =
  | Program of location * import list * block
  | Block of location * astNode list
  | IfExpression of location * astNode * astNode * astNode
  | ForeachExpression of location * string * astNode * astNode
  | BinaryExpression of location * operator * astNode * astNode
  | WhileExpression of location * astNode * astNode
  | MinusOp of location * astNode
  | PlusOp of location * astNode
  | StringNode of location * string
  | IntNode of location * int
  | LongNode of location * long
  | ShortNode of location * short
  | ByteNode of location * byte
  | BooleanNode of location * bool
  | DoubleNode of location * double
  | FloatNode of location * float
  | Identifier of location * string
  | Assignment of location * string * astNode
  | ValDeclaration of location * string * typeDescription option * astNode
  | FunctionLiteral of location * formalParameter list * astNode
  | FunctionDefinition of location * string * functionLiteral
  | FunctionCall of location * astNode * astNode list
  | ListLiteral of location * astNode list
  | NewObject of location * string * astNode list
  | MethodCall of location * astNode * string * astNode list
and block = astNode list
and functionLiteral = formalParameter list * astNode

let get_location = function
  | Program(location,_,_) -> location
  | Block(location,_) -> location
  | IfExpression(location,_,_,_) -> location
  | ForeachExpression(location,_,_,_) -> location
  | BinaryExpression(location,_,_,_) -> location
  | WhileExpression(location,_,_) -> location
  | MinusOp(location,_) -> location
  | PlusOp(location,_) -> location
  | StringNode(location,_) -> location
  | IntNode(location,_) -> location
  | LongNode(location,_) -> location
  | ShortNode(location,_) -> location
  | ByteNode(location,_) -> location
  | BooleanNode(location,_) -> location
  | DoubleNode(location,_) -> location
  | FloatNode(location,_) -> location
  | Identifier(location,_) -> location
  | Assignment(location,_,_) -> location
  | ValDeclaration(location,_,_,_) -> location
  | FunctionLiteral(location,_,_) -> location
  | FunctionDefinition(location,_,_) -> location
  | FunctionCall(location,_,_) -> location
  | ListLiteral(location,_) -> location
  | NewObject(location,_,_) -> location
  | MethodCall(location,_,_,_) -> location

let get_import_location = function
  | Import(location,_,_) -> location

(* todo *)
let show_functionLiteral v = "functionLiteral"

let show v = "astNode"

(* todo make show function *)
(*function
  | Program(location, imports, block) -> Printf.sprintf "Program(%s, %s, %s)" (show_location location) (show_imports imports) (show_block block)
  | Block(location, astNodes)
  | IfExpression(location, astNode, astNode, astNode
  | ForeachExpression(location, string, astNode, astNode
  | BinaryExpression(location, operator, astNode, astNode
  | WhileExpression(location, astNode, astNode
  | MinusOp(location, astNode
  | PlusOp(location, astNode
  | StringNode(location, string
  | IntNode(location, int
  | LongNode(location, long
  | ShortNode(location, short
  | ByteNode(location, byte
  | BooleanNode(location, bool
  | DoubleNode(location, double
  | FloatNode(location, float
  | Identifier(location, string
  | Assignment(location, string, astNode
  | ValDeclaration(location, string, typeDescription option, astNode
  | FunctionLiteral(location, formalParameter list, astNode
  | FunctionDefinition(location, string, functionLiteral
  | FunctionCall(location, astNode, astNode list
  | ListLiteral(location, astNode list
  | NewObject(location, string, astNode list
  | MethodCall(location, astNode, string, astNode list
*)