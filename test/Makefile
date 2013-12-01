.SUFFIXES:
.PHONY: build test

OCAMLBUILD=ocamlbuild

default: test

test: build
	./test.native

build:
	$(OCAMLBUILD) -use-ocamlfind test.native

clean:
	$(OCAMLBUILD) -clean
