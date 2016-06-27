{
open Parser

let keywords = [
  "<"; ">"; "<="; ">="; "+"; "-"; "*"; "/"; "{"; "}"; "["; "]"; ":"; "?";
  "if"; "else"; "while"; "foreach"; "import"; "true"; "false"; "in"; ";"; ".";
  "class"; "def"; "val"; "="; "=="; "=>"; "new"; "&&"; "||"]
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let linefeed = ("\r\n" | "\r" | "\n")

(*
  lazy val SEMICOLON: Parser[String] = ";"
  lazy val ANY: Parser[String] = elem(".", (ch: Char) => ch != CharSequenceReader.EofCh) ^^ {_.toString}
  def CL[T](parser: Parser[T]): Parser[T] = parser <~ SPACING

  lazy val SPACING: Parser[String] = (COMMENT | "\r\n" | "\r" | "\n" | " " | "\t" | "\b" | "\f").* ^^ {_.mkString}
  lazy val SPACING_WITHOUT_LF: Parser[String] = (COMMENT | "\t" | " " | "\b" | "\f").* ^^ {_.mkString}
  lazy val TERMINATOR: Parser[String] = (linefeed | SEMICOLON | EOF) <~ SPACING
  lazy val SEPARATOR: Parser[String] = (linefeed | COMMA | EOF | SPACING_WITHOUT_LF) <~ SPACING
  %token BLOCK_COMMENT
  %token LINE_COMMENT

  lazy val COMMENT: Parser[Any] = BLOCK_COMMENT | LINE_COMMENT
*)

rule token = parse
| space+    { token lexbuf }
| "/*"      { comment lexbuf; token lexbuf }
| "Byte"    { BYTE_TYPE    }
| "Short"   { SHORT_TYPE   }
| "Int"     { INT_TYPE     }
| "Long"    { LONG_TYPE    }
| "Float"   { FLOAT_TYPE   }
| "Double"  { DOUBLE_TYPE  }
| "Boolean" { BOOLEAN_TYPE }
| "Unit"    { UNIT_TYPE    }
| "if"      { IF           }
| "else"    { ELSE         }
| "while"   { WHILE        }
| "foreach" { FOREACH      }
| "import"  { IMPORT       }
| "true"    { TRUE         }
| "false"   { FALSE        }
| "in"      { IN           }
| "class"   { CLASS        }
| "def"     { DEF          }
| "val"     { VAL          }
| "<"       { LT           }
| ">"       { GT           }
| "<="      { LTE          }
| ">="      { GTE          }
| "+"       { PLUS         }
| "-"       { MINUS        }
| "*"       { ASTER        }
| "/"       { SLASH        }
| "("       { LPAREN       }
| ")"       { RPAREN       }
| "{"       { LBRACE       }
| "}"       { RBRACE       }
| "["       { LBRACKET     }
| "]"       { RBRACKET     }
| ","       { COMMA        }
| "."       { DOT          }
| "="       { EQ           }
| "=="      { EQEQ         }
| "=>"      { ARROW        }
| ":"       { COLON        }
| "new"     { NEW          }
| "?"       { QUES         }
| "&&"      { AMP2         }
| "||"      { BAR2         }
| (['1'-'9']['0'-'9']*|'0') "BY" { BYTE (int_of_string (Lexing.lexeme lexbuf)) }
| (['1'-'9']['0'-'9']*|'0') "L"  { LONG (int_of_string (Lexing.lexeme lexbuf)) }
| (['1'-'9']['0'-'9']*|'0') "S"  { SHORT(int_of_string (Lexing.lexeme lexbuf)) }
| (['1'-'9']['0'-'9']*|'0')      { INT  (int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)? 'F' { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)? { DOUBLE(float_of_string (Lexing.lexeme lexbuf)) }
| '"' (([^ '"' '\\'] | '\\' ['r' 'n' 't' 'f' 'b' '"' '\'' '\\'] )* as s) '"' {  STRING s }

(* stringLiteral ::= "\"" ((?!")(\[rntfb"'\\]|[^\\]))* "\"" *)
(*
stringLiteral :    |
  "\""
    ( ( (?!("|#\{))(\\[rntfb"'\\]|[^\\]))+ {  StringNode(location(), unescape($1)) } | "#{" expression "}"
    )*
  "\"" SPACING_WITHOUT_LF {
  $1.foldLeft(StringNode(NoLocation, "")) { (node, content) => BinaryExpression(content.location, Operator.ADD, node, content) }
}
*)

| eof       { EOF }
| ['A'-'Z' 'a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9']*
            { IDENT(Lexing.lexeme lexbuf) }
| _         { failwith
                (Printf.sprintf "unknown token %s near characters %d-%d"
                   (Lexing.lexeme lexbuf)
                   (Lexing.lexeme_start lexbuf)
                   (Lexing.lexeme_end lexbuf)) }

and comment = parse
| "*/"      { () }
| "/*"      { comment lexbuf; comment lexbuf }
| eof       { Format.eprintf "warning: unterminated comment@." }
| _         { comment lexbuf }
