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




install: lib

	mkdir -p $(OCAML_LIBDIR)
	$(OCAML_FIND) install ordma -destdir $(OCAML_LIBDIR) META \
	  _build/rsocket.mli \
	  _build/rsocket.cmi \
	  _build/lwt_rsocket.mli \
	  _build/lwt_rsocket.cmi \
	  _build/libordma_c.a \
	  _build/libordma.a \
	  _build/libordma.cmxa \
	  _build/libordma.cmxs

uninstall:
	$(OCAML_FIND) remove ordma -destdir $(OCAML_LIBDIR)


test: 
	cd test && ocamlbuild -use-ocamlfind lwt_test.native test.native
