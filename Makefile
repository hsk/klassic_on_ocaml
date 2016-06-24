all:
	ocamlc TypeDescription.ml Operator.ml FormalParameter.ml LanguageException.ml AstNode.ml Value.ml Environment.ml
	rm *.cm* a.out

clean:
	rm *.cm* a.out
