all: alac.native

alac.native: arrayTypes.cmi matrix.cmx dynamicPredictor.cmx bitBuffer.cmx adaptiveGolomb.cmx mp4.cmx alac.cmx
	ocamlfind ocamlopt -package bitstring,bigarray -linkpkg $(filter-out %.cmi, $+) -o $@

clean:
	rm -f *.cm[ix] *.o alac.native

%.cmx: %.ml
	ocamlfind ocamlopt -package bitstring,bitstring.syntax -syntax camlp4o,bitstring.syntax -c $< -o $@

%.cmi: %.mli
	ocamlfind ocamlopt -c $< -o $@
