JOBS ?= 1

DEP = *.ml parsers/*.ml parsers/*.mll parsers/*.mly
OCB = ocamlbuild -use-ocamlfind -j $(JOBS)

# TODO use ocamlbuild for js_of_ocaml
JSC = js_of_ocaml --disable genprim --pretty +weak.js

.PHONY: default
default: Main.native

.PHONY: all
all: default ide Fuzz.native

##
## executables
##

%.native: %.ml $(DEP)
	$(OCB) $@

%.inferred.mli: %.ml $(DEP)
	$(OCB) $@

##
## ide
##

%.js: %.byte $(DEP)
	$(JSC) $<

ide/JSLib.js: JSLib.js
	cp $< ide/

.PHONY: ide
ide: ide/JSLib.js

##
## clean
##

.PHONY: clean
clean:
	rm -f JSLib.js
	ocamlbuild -clean
	@echo

.PHONY: clean-tmp
clean-tmp:
	rm -rf tmp/*

.PHONY: clean-all
clean-all: clean clean-doc clean-tmp
