exception LanguageException of string
exception InterpreterException of string
exception InterpreterPanic of string

let interapterPanic message = raise (InterpreterPanic ("[PANIC]:" ^ message))
