OCAMLFIND=ocamlfind
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
PROGNAME=abs
EXTLIB=extlib
CIL=cil
PACKAGE=cil,extlib


# The list of .cmx files for trans
CMXS= uexception.cmx utype.cmx main.cmx abs.cmx trs.ml oma.cmx arec.cmx

install: $(CMXS)
	$(OCAMLFIND) $(OCAMLOPT) -o $(PROGNAME) -linkpkg -package $(PACKAGE) $(CMXS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT)  -c -linkpkg  -package $(PACKAGE) $<	

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox]  *.o *~ *.out rewritten_file2.c rewritten_file.c rewritten_file1.c
	rm -f *#*#

