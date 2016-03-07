.PHONY: all
all: src/pego.cma

.PHONY: install
install: src/pego.cma src/pego.cmxa
	ocamlfind install pego META _build/src/pego.*

.PHONY: uninstall
uninstall:
	ocamlfind remove pego

src/pego.cma: src/*.ml
	ocamlbuild $@

src/pego.cmxa: src/*.ml
	ocamlbuild $@

