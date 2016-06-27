%{
open AstNode
open TypeDescription

let unescape(input: string): string = 
  let length = String.length input in
  let builder = Buffer.create (length * 2) in
  let i = ref 0 in
  while i < length - 1 do
    match (String.get i input, String.get (i+1) input) with
    | ('\\', 'r') -> Buffer.add_char builder '\r'; i := !i + 2
    | ('\\', 'n') -> Buffer.add_char builder '\n'; i := !i + 2
    | ('\\', 'b') -> Buffer.add_char builder '\b'; i := !i + 2
    | ('\\', 'f') -> Buffer.add_char builder '\f'; i := !i + 2
    | ('\\', 't') -> Buffer.add_char builder '\t'; i := !i + 2
    | ('\\', '\\') -> Buffer.add_char builder '\\'; i := !i + 2
    | (ch, _)      -> Buffer.add_char builder ch; i := !i + 1
  done;
  if i = length - 1 then
    Buffer.add_char builder (String.get i input) ;
  Buffer.contents builder

let location () = NoLocation
%}
%token <int> INT
%token <byte> BYTE
%token <short> SHORT
%token <long> LONG
%token <float> FLOAT
%token <float> DOUBLE
%token <string> IDENT
%token <string> STRING
%token EOF
%token BYTE_TYPE
%token SHORT_TYPE
%token INT_TYPE
%token LONG_TYPE
%token FLOAT_TYPE
%token DOUBLE_TYPE
%token BOOLEAN_TYPE
%token UNIT_TYPE
%token L

%token IF      
%token ELSE    
%token WHILE   
%token FOREACH 
%token IMPORT  
%token TRUE    
%token FALSE   
%token IN      
%token CLASS   
%token DEF     
%token VAL     

%token LT      
%token GT      
%token LTE     
%token GTE     
%token PLUS    
%token MINUS   
%token ASTER   
%token SLASH   
%token LPAREN  
%token RPAREN  
%token LBRACE  
%token RBRACE  
%token LBRACKET
%token RBRACKET
%token COMMA   
%token DOT     
%token EQ      
%token EQEQ    
%token ARROW   
%token COLON   
%token NEW     
%token QUES    
%token AMP2    
%token BAR2    
%token TERMINATOR
%token SEPARATOR
%token SPACING
%token SPACING_WITHOUT_LF

%type <TypeDescription.typeDescription> typeAnnotation
%start typeAnnotation

%type <AstNode.astNode> expression
%start expression

%%


token :            | SPACING_WITHOUT_LF { () }

typeAnnotation :   | COLON BYTE_TYPE    token { ByteType }
                   | COLON SHORT_TYPE   token { ShortType }
                   | COLON INT_TYPE     token { IntType }
                   | COLON LONG_TYPE    token { LongType }
                   | COLON FLOAT_TYPE   token { FloatType }
                   | COLON DOUBLE_TYPE  token { DoubleType }
                   | COLON BOOLEAN_TYPE token { BooleanType }
                   | COLON UNIT_TYPE    token { UnitType }
                   | COLON QUES         token { UnknownType }
                   | COLON ASTER        token { DynamicType }

program:           | SPACING imports lines            { Program(location(), $2, $3) }
                   | SPACING imports lines TERMINATOR { Program(location(), $2, $3) }


imports:           |                           { [] }
                   | import                    { [$1] }
                   | import TERMINATOR imports { $1::$3 }

import:            | IMPORT fqcn               { Import(location(), List.nth $2 (List.length $2 - 1), String.concat "." $2) }

lines:             | SPACING lines1            { Block(location(), $2) }
                   | SPACING lines1 TERMINATOR { Block(location(), $2) }
lines1:            |                           { [] }
                   | line                      { [$1] }
                   | line TERMINATOR lines1    { $1::$3 }

line:              | expression                { $1 }
                   | val_declaration           { $1 }
                   | functionDefinition        { $1 }

expression:        | assignment                { $1 }
                   | logical                   { $1 }
                   | ifExp                     { $1 }
                   | whileExp                  { $1 }
                   | foreach                   { $1 }

ifExp:             | IF LPAREN expression RPAREN expression ELSE expression { IfExpression(location(), $3, $5, $7) }
whileExp:          | WHILE LPAREN expression RPAREN expression              { WhileExpression(location(), $3, $5) }

foreach:           | FOREACH LPAREN ident2 IN expression RPAREN expression  { ForeachExpression(location(), $3, $5, $7) }
logical:           | conditional                  { $1 }
                   | conditional AMP2 conditional { BinaryExpression(location(), Operator.AND2, $1, $3) }
                   | conditional BAR2 conditional { BinaryExpression(location(), Operator.BAR2, $1, $3) }
conditional:       | add          { $1 }
                   | add EQEQ add { BinaryExpression(location(), Operator.EQUAL, $1, $3) }
                   | add LTE  add { BinaryExpression(location(), Operator.LESS_OR_EQUAL, $1, $3) }
                   | add GTE  add { BinaryExpression(location(), Operator.GREATER_EQUAL, $1, $3) }
                   | add LT   add { BinaryExpression(location(), Operator.LESS_THAN, $1, $3) }
                   | add GT   add { BinaryExpression(location(), Operator.GREATER_THAN, $1, $3) }

add:               | term            { $1 }
                   | term PLUS  term { BinaryExpression(location(), Operator.ADD, $1, $3) }
                   | term MINUS term { BinaryExpression(location(), Operator.SUBTRACT, $1, $3) }

term:              | unary             { $1 }
                   | unary ASTER unary { BinaryExpression(location(), Operator.MULTIPLY, $1, $3) }
                   | unary SLASH unary { BinaryExpression(location(), Operator.DIVIDE, $1, $3) }

unary:             | invocation       { $1 }
                   | MINUS invocation { MinusOp(location(), $2) }
                   | PLUS  invocation { PlusOp(location(), $2) }

invocation:        | application { $1 }
                   | application invocation1s { List.fold_left (fun self (name, params) -> MethodCall(location(), self, name, params)) $1 $2 }

invocation1s:      |                                      { [] }
                   | invocation1                          { [$1] }
                   | invocation1 invocation1s             { $1::$2 }
invocation1:       | DOT ident2                           { ($2, []) }
                   | DOT ident2 LPAREN expressions RPAREN { ($2, $4) }

application:       | primary                                   { $1 }
                   | primary LPAREN expressions SPACING RPAREN { FunctionCall(location(), $1, $3) }

expressions:       |                              { [] }
                   | expression                   { [$1] }
                   | expression COMMA expressions { $1::$3 }

primary:           | ident                    { $1 }
                   | floatLiteral             { $1 }
                   | integerLiteral           { $1 }
                   | stringLiteral            { $1 }
                   | listLiteral              { $1 }
                   | newObject                { $1 }
                   | anonymousFunction        { $1 }
                   | LPAREN expression RPAREN { $2 }
                   | LBRACE lines RBRACE      { $2 }/*
                   | hereDocument             { $1 }
*/


integerLiteral:    | INT   { IntNode($1) }
                   | LONG  { LongNode($1) }
                   | SHORT { ShortNode($1) }
                   | BYTE  { ByteNode($1) }

floatLiteral:      | FLOAT  { FloatNode(location(), $1) }
                   | DOUBLE { DoubleNode(location(), $1) }

booleanLiteral:    | TRUE  { BooleanNode(location(), true)  }
                   | FALSE { BooleanNode(location(), false) }

stringLiteral:     | STRING { StringNode(location(), unescape($1)) }
listLiteral:       | LBRACKET list SEPARATOR RBRACKET { ListLiteral(location(), $2) }
                   | LBRACKET list           RBRACKET { ListLiteral(location(), $2) }

list:              | { [] }
                   | expression { [$1] }
                   | expression SEPARATOR list { $1::$3 }

fqcn:              | ident2          { [$1] }
                   | ident2 DOT fqcn { $1 :: $3 }
ident2:            | IDENT SPACING_WITHOUT_LF                { $1 }
ident:             | IDENT SPACING_WITHOUT_LF                { Identifier(location(), $1) }
assignment:        | IDENT SPACING_WITHOUT_LF EQ expression  { Assignment(location(), $1, $4) }

val_declaration:   | VAL ident2 typeAnnotation EQ expression { ValDeclaration(location(), $2, Some($3), $5) }
                   | VAL ident2                EQ expression { ValDeclaration(location(), $2,     None, $4) }

anonymousFunction: | ARROW expression { FunctionLiteral(location(), List(), $2) }
anonymousFunction: | LPAREN params RPAREN ARROW expression { FunctionLiteral(location(), $2, $5) }

params:            | { [] }
                   | ident2 { formalParameter($1) }
                   | ident2 typeAnnotation { FormalParameter($1, $2) }
                   | ident2 COMMA params { formalParameter($1)::$3 }
                   | ident2 typeAnnotation COMMA params { FormalParameter($1, $2)::$4 }

newObject:         | NEW fqcn                      { NewObject(location(), String.concat "." $2, []) }
newObject:         | NEW fqcn LPAREN idents RPAREN { NewObject(location(), String.concat "." $2, $4) }
idents:            |                     { [] }
                   | ident2              { [$1] }
                   | ident2 COMMA idents { $1::$3 }

opt_typeAnnotation:| { DynamicType }
                   | typeAnnotation { $1 }

functionDefinition:| DEF ident2 opt_typeAnnotation EQ expression
                     { FunctionDefinition(location(), $2, FunctionLiteral(location(), [], $5)) }
                   | DEF ident2 LPAREN params RPAREN opt_typeAnnotation EQ expression
                     { FunctionDefinition(location(), $2, FunctionLiteral(location(), $4, $8)) }
                    
/*def parse(str:String): ParseResult[AstNode] = parseAll(lines, str)*/