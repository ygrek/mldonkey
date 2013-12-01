.SUFFIXES:
.PHONY: build test

OCAMLBUILD=ocamlbuild

default: test

test: build
	./test.native

build:
	$(OCAMLBUILD) -use-ocamlfind -pkg oUnit test.native

clean:
	$(OCAMLBUILD) -clean
