PPX_NAME    = ppx_letif
PPX_FOLDER  = ppx
PPX_BINARY  = $(PPX_FOLDER)/$(PPX_NAME)
PPX_SOURCES = $(wildcard $(PPX_FOLDER)/*.ml)
PPX_CMX     = $(PPX_SOURCES:.ml=.cmx)
PPX_FLAGS   = -I ppx -I +compiler-libs

all: $(PPX_BINARY)

$(PPX_BINARY): $(PPX_CMX)
	ocamlfind ocamlopt $(PPX_FLAGS) -o $@ ocamlcommon.cmxa $^

.SUFFIXES: .ml .mli .cmi .cmx

.mli.cmi:
	ocamlfind ocamlopt $(PPX_FLAGS) -c $<

.ml.cmx:
	ocamlfind ocamlopt $(PPX_FLAGS) -c $<

.PHONY: clean
clean:
	rm -f $(PPX_BINARY) $(PPX_CMX) $$(find . -name "*.cmi" -o -name "*.o")
