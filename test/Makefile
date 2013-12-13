.SUFFIXES:
.PHONY: build test

OPTIONS=-g -annot -w +a-4

default: test

test: build
	./test.native

build:
	ocamlfind ocamlopt -linkpkg -package curl,oUnit,extlib $(OPTIONS) prelude.ml web.ml test_collector.ml $(wildcard *_test.ml) test.ml -o test.native

clean:
	rm -f *.cmx *.cmo *.cmi *.o *.native *.byte *.annot
