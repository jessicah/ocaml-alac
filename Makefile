all: alac.native

alac.native: arrayTypes.cmi bigutils.cmxa matrix.cmx dynamicPredictor.cmx bitBuffer.cmx adaptiveGolomb.cmx mp4.cmx alac.cmx
	ocamlfind ocamlopt -package bitstring,bigarray -linkpkg $(filter-out %.cmi, $+) -I . -o $@

bigutils.cmxa: arrayTypes.cmi bigarray_extra_stubs.o bigarrayUtils.cmx
	ocamlmklib -failsafe $(filter-out %.cmi, $+) -o $(basename $@)

clean:
	rm -f *.cm[aoix] *.cmxa *.[ao] alac.native

%.cmx: %.ml
	ocamlfind ocamlopt -package bitstring,bitstring.syntax -syntax camlp4o,bitstring.syntax -c $< -o $@

%.cmi: %.mli
	ocamlfind ocamlopt -c $< -o $@

%.o: %.c
	ocamlfind ocamlopt -c $< -o $@

