all: alac.native

alac.native: alac.ml matrix.ml dynamicPredictor.ml bitBuffer.ml arrayTypes.mli
	ocamlbuild -tag use_bigarray alac.native

clean:
	ocamlbuild -clean

