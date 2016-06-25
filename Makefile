all:
	ocamlyacc Parser.mly
	rm -rf Parser.mli
	ocamllex Lexer.mll
	ocamlc TypeDescription.ml Operator.ml FormalParameter.ml LanguageException.ml AstNode.ml Value.ml Environment.ml
	rm -rf *.cm* a.out Parser.ml Parser.mli Lexer.ml

clean:
	rm -rf *.cm* a.out Parser.ml Parser.mli Lexer.ml
