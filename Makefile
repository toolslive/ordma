OCAML_LIBDIR ?=`ocamlfind printconf destdir`
OCAML_FIND ?= ocamlfind

lib:
	ocamlbuild -use-ocamlfind \
	libordma_c.a \
	libordma.cma \
	libordma.cmxa \
	libordma.cmxs

clean:
	rm -f *.o *.annot *.cm*
	ocamlbuild -clean
	cd test && ocamlbuild -clean



install: lib

	mkdir -p $(OCAML_LIBDIR)
	$(OCAML_FIND) install ordma -destdir $(OCAML_LIBDIR) _build/META \
	  _build/rsocket.mli \
	  _build/rsocket.cmi \
	  _build/rsocket.cmx \
	  _build/lwt_rsocket.mli \
	  _build/lwt_rsocket.cmi \
	  _build/lwt_rsocket.cmx \
	  _build/libordma_c.a \
	  _build/libordma.a \
	  _build/libordma.cma \
	  _build/libordma.cmxa \
	  _build/libordma.cmxs \
	  _build/dllordma_c.so

uninstall:
	$(OCAML_FIND) remove ordma -destdir $(OCAML_LIBDIR)


test:
	cd test && ocamlbuild -use-ocamlfind lwt_test.native test.native

.PHONY: test
