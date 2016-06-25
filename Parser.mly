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
%token <float> FLOAT
%token <string> IDENT
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


token : | SPACING_WITHOUT_LF { () }

typeAnnotation : | COLON BYTE_TYPE    token { ByteType }
                 | COLON SHORT_TYPE   token { ShortType }
                 | COLON INT_TYPE     token { IntType }
                 | COLON LONG_TYPE    token { LongType }
                 | COLON FLOAT_TYPE   token { FloatType }
                 | COLON DOUBLE_TYPE  token { DoubleType }
                 | COLON BOOLEAN_TYPE token { BooleanType }
                 | COLON UNIT_TYPE    token { UnitType }
                 | COLON QUES         token { UnknownType }
                 | COLON ASTER        token { DynamicType }

/*
  def program: Parser[Program] = (SPACING ~> %) ~ repsep(`import`, TERMINATOR) ~ (lines <~ opt(TERMINATOR)) ^^ {
    case location ~ imports ~ block => Program(location, imports, block)
  }

  def `import`: Parser[Import] = (% <~ CL(IMPORT)) ~ fqcn ^^ { case location ~ fqcn =>
    val fragments = fqcn.split(".")
    Import(location, fragments(fragments.length - 1), fqcn)
  }

  //lines ::= line {TERMINATOR expr} [TERMINATOR]
  def lines: Parser[Block] = SPACING ~> (% ~ repsep(line, TERMINATOR)) <~ opt(TERMINATOR) ^^ { case location ~ expressions =>
      Block(location, expressions)
  }

  //line ::= expression | val_declaration | functionDefinition
  def line: Parser[AstNode] = expression | val_declaration | functionDefinition

  //expression ::= assignment | conditional | if | while
  */

expression:   | assignment        { $1 }
              | logical           { $1 }
              | ifExpression      { $1 }
              | whileExp          { $1 }
              | foreach           { $1 }

ifExpression:  | IF LPAREN expression RPAREN expression ELSE expression { IfExpression(location(), $3, $5, $7) }
whileExp:      | WHILE LPAREN expression RPAREN expression { WhileExpression(location(), $3, $5) }

foreach:       | FOREACH LPAREN ident2 IN expression RPAREN expression { ForeachExpression(location(), $3, $5, $7) }
logical:       | conditional { $1 }
               | conditional AMP2 conditional { BinaryExpression(location(), Operator.AND2, $1, $3) }
               | conditional BAR2 conditional { BinaryExpression(location(), Operator.BAR2, $1, $3) }
conditional:   | add { $1 }
               | add EQEQ add { BinaryExpression(location(), Operator.EQUAL, $1, $3) }
               | add LTE add { BinaryExpression(location(), Operator.LESS_OR_EQUAL, $1, $3) }
               | add GTE add { BinaryExpression(location(), Operator.GREATER_EQUAL, $1, $3) }
               | add LT add { BinaryExpression(location(), Operator.LESS_THAN, $1, $3) }
               | add GT add { BinaryExpression(location(), Operator.GREATER_THAN, $1, $3) }

add:           | term { $1 }
               | term PLUS term { BinaryExpression(location(), Operator.ADD, $1, $3) }
               | term MINUS term { BinaryExpression(location(), Operator.SUBTRACT, $1, $3) }

term:          | unary { $1 }
               | unary ASTER unary { BinaryExpression(location(), Operator.MULTIPLY, $1, $3) }
               | unary SLASH unary { BinaryExpression(location(), Operator.DIVIDE, $1, $3) }

unary:         | invocation { $1 }
               | MINUS invocation { MinusOp(location(), $2) }
               | PLUS  invocation { PlusOp(location(), $2) }


invocation:    | application { $1 }
/*
  def invocation: Parser[AstNode] = % ~ application ~ ((CL(DOT) ~> ident) ~ opt(CL(LPAREN) ~> repsep(expression, CL(COMMA)) <~ RPAREN)).* ^^ {
    case location ~ self ~ Nil =>
      self
    case location ~ self ~ npList  =>
      npList.foldLeft(self){case (self, name ~ params) => MethodCall(location, self, name.name, params.getOrElse(Nil))}
  }
*/
application:   | primary  { $1 }
               | primary LPAREN expressions SPACING RPAREN { FunctionCall(location(), $1, $3) }

expressions:   | { [] }
               | expression { [$1] }
               | expression COMMA expressions { $1::$3 }

/*
  def application: Parser[AstNode] = % ~ primary ~ opt(CL(LPAREN) ~> repsep(CL(expression), CL(COMMA)) <~ (SPACING <~ RPAREN))^^ {
    case location ~ fac ~ param => {
      param match {
        case Some(p) =>
          FunctionCall(location, fac, p)
        case None =>
          fac
      }
    }
  }
*/

primary:       | ident { $1 }
/*             | floatLiteral { $1 }
               | integerLiteral { $1 }
               | stringLiteral { $1 }
               | listLiteral { $1 }
               | newObject { $1 }
               | anonymousFunction { $1 }
               | LPAREN expression RPAREN { $2 }
               | LBRACE lines RBRACE { $2 }
               | hereDocument { $1 }*/
/*
  //primary ::= intLiteral | stringLiteral | listLiteral | "(" expression ")" | "{" lines "}"
  def primary: Parser[AstNode] = ident | floatLiteral | integerLiteral | stringLiteral | listLiteral | newObject | anonymousFunction | CL(LPAREN) ~>expression<~ RPAREN | CL(LBRACE) ~>lines<~ RBRACE | hereDocument
*/

/*
  //intLiteral ::= ["1"-"9"] {"0"-"9"}
  def integerLiteral : Parser[AstNode] = (% ~ """[1-9][0-9]*|0""".r ~ opt("BY"| "L" | "S") ^^ {
    case location ~ value ~ None => IntNode(location, value.toLong.toInt)
    case location ~ value ~ Some("L") => LongNode(location, value.toLong)
    case location ~ value ~ Some("S") => ShortNode(location, value.toShort)
    case location ~ value ~ Some("BY") => ByteNode(location, value.toByte)
  }) <~ SPACING_WITHOUT_LF

  def floatLiteral: Parser[AstNode]= (% ~ "([1-9][0-9]*|0)\\.[0-9]*".r ~ opt("F")) ^^ {
    case location ~ value ~ None => DoubleNode(location, value.toDouble)
    case location ~ value ~ Some("F") => FloatNode(location, value.toFloat)
  }

  def booleanLiteral: Parser[AstNode] = % ~ (TRUE | FALSE) ^^ {
    case location ~ "true" => BooleanNode(location, true)
    case location ~ "false" => BooleanNode(location, false)
  }

  //stringLiteral ::= "\"" ((?!")(\[rntfb"'\\]|[^\\]))* "\""
  def stringLiteral : Parser[AstNode] =
    ("\"" ~>
      (% ~ """((?!("|#\{))(\\[rntfb"'\\]|[^\\]))+""".r ^^ {case location ~ in =>
        StringNode(location, unescape(in))
      } | "#{" ~> expression <~ "}"
      ).*
    <~ "\"" ^^ { values =>
    values.foldLeft(StringNode(NoLocation, ""):AstNode) { (node, content) => BinaryExpression(content.location, Operator.ADD, node, content) }
  }) <~ SPACING_WITHOUT_LF

  def listLiteral: Parser[AstNode] = % ~ (CL(LBRACKET) ~> (repsep(CL(expression), SEPARATOR) <~ opt(SEPARATOR)) <~ RBRACKET) ^^ {
    case location ~ contents => ListLiteral(location, contents)
  }

  def fqcn: Parser[String] = (ident ~ (CL(DOT) ~ ident).*) ^^ {
    case id ~ ids => ids.foldLeft(id.name){ case (a, d ~ e) => a + d + e.name}
  }

  def rebuild(a: Reader[Char], newSource: String, newOffset: Int): Reader[Char] = new Reader[Char] {
    def atEnd = a.atEnd
    def first = a.first
    def pos = a.pos
    def rest = rebuild(a.rest, newSource, offset + 1)
    override def source = newSource
    override def offset = newOffset
  }

  def cat(a: Reader[Char], b: Reader[Char]): Reader[Char] = {
    val aSource = a.source + b.source.subSequence(b.offset, b.source.length()).toString
    if(a.atEnd) {
      rebuild(b, aSource, a.offset)
    } else {
      new Reader[Char] {
        private lazy val result = cat(a.rest, b)
        def atEnd = a.atEnd
        def first = a.first
        def pos = a.pos
        def rest = result
        override def source = aSource
        override def offset = a.offset
      }
    }
  }

  lazy val oneLine: Parser[String] = regex(""".*(\r\n|\r|\n|$)""".r)

  lazy val hereDocument: Parser[StringNode] = ("""<<[a-zA-Z_][a-zA-Z0-9_]*""".r >> { t =>
    val tag = t.substring(2)
    Parser{in =>
      val Success(temp, rest) = oneLine(in)

      val line = new CharSequenceReader(temp, 0)
      hereDocumentBody(tag).apply(rest) match {
        case Success(value, next) =>
          val source = cat(line, next)
          Success(StringNode(NoLocation, value), source)
        case Failure(msg, next) => Failure(msg, cat(line, next))
        case Error(msg, next) => Error(msg, cat(line, next))
      }
    }
  }) <~ SPACING_WITHOUT_LF

  def hereDocumentBody(beginTag: String): Parser[String] = oneLine >> {line =>
    if(beginTag == line.trim) "" else hereDocumentBody(beginTag) ^^ {result =>
      line + result
    }
  }
*/
ident2:     | IDENT SPACING_WITHOUT_LF               { $1 }
ident:      | IDENT SPACING_WITHOUT_LF               { Identifier(location(), $1) }
assignment: | IDENT SPACING_WITHOUT_LF EQ expression { Assignment(location(), $1, $4) }

/*
  // val_declaration ::= "val" ident "=" expression
  def val_declaration:Parser[ValDeclaration] = ((% <~ CL(VAL)) ~ ident ~ opt(typeAnnotation) <~ CL(EQ)) ~ expression ^^ {
    case location ~ valName ~ optionalType ~ value => ValDeclaration(location, valName.name, optionalType, value)
  }

  // anonnymousFunction ::= "(" [param {"," param}] ")" "=>" expression
  def anonymousFunction():Parser[AstNode] = % ~ (opt(CL(LPAREN) ~> repsep(ident ~ opt(typeAnnotation), CL(COMMA)) <~ CL(RPAREN)) <~ CL(ARROW)) ~ expression ^^ {
    case location ~ Some(params) ~ body =>
      FunctionLiteral(
        location,
        params.map {
          case name ~ Some(description) => FormalParameter(name.name, description)
          case name ~ None => FormalParameter(name.name)
        }, body
      )
    case location ~ None ~ body => FunctionLiteral(location, List(), body)
  }

  // newObject ::= "new" fqcn "(" [param {"," param} ")"
  def newObject: Parser[AstNode] = (% <~ CL(NEW)) ~ fqcn ~ (opt(CL(LPAREN) ~> repsep(ident, CL(COMMA)) <~ (RPAREN))) ^^ {
    case location ~ className ~ Some(params) => NewObject(location, className, params)
    case location ~ className ~ None => NewObject(location, className, List())
  }

  // functionDefinition ::= "def" ident  ["(" [param {"," param]] ")"] "=" expression
  def functionDefinition:Parser[FunctionDefinition] = (% <~ CL(DEF)) ~ ident ~ opt(CL(LPAREN) ~>repsep(ident ~ opt(typeAnnotation), CL(COMMA)) <~ CL(RPAREN)) ~ opt(typeAnnotation) ~ CL(EQ) ~ expression ^^ {
    case location ~ functionName ~ params ~ _ ~ optionalType ~ body =>
      val ps = params match {
        case Some(xs) =>
          xs.map{
            case name ~ Some(annotation) => FormalParameter(name.name, annotation)
            case name ~ None => FormalParameter(name.name)
          }
        case None => Nil
      }
      FunctionDefinition(
        location,
        functionName.name,
        FunctionLiteral(body.location, ps, body)
      )
  }

  def parse(str:String): ParseResult[AstNode] = parseAll(lines, str)
}

*/