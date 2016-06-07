OCAMLFIND=ocamlfind
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
PROGNAME=abs
CIL=cil

# The list of .cmx files for trans
CMXS= uexception.cmx  main.cmx abs.cmx oma.cmx

install: $(CMXS)
	$(OCAMLFIND) $(OCAMLOPT) -o $(PROGNAME) -linkpkg -package $(CIL) $(CMXS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT)  -c -package $(CIL) $<

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox]  *.o *~ *.out
	rm -f *#*#
