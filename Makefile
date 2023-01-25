#
# Straightforward picoml compilation using ocamlbuild 
# (without ocamlbuild use Makefiles/Makefile*)
#
SOURCES = $(shell echo *.ml scanner.mul *.mly)
LIST    = $(subst scanner.ml,,$(SOURCES))
#
#
#
picoml:         $(SOURCES);\
                make scanner.ml;\
                ocamlbuild -use-ocamlfind main.native; ln -f main.native picoml; rm main.native

clean:          ;\
                rm -rf _build scanner.ml

cleaner: clean  ;\
                rm -rf picoml
                
listing:        $(LIST); list -p -s=9 -c $(LIST)
                
#####################################################################################
#
#       Make the lexer automaton with ulex
#
ULEX    = $(shell ocamlfind query ulex) 
PKG     = -I $(ULEX)
#
#       Expand the lexer rules syntax using our local scanner-generator
#
scanner.ml:    scanner.mul; \
               camlp4o $(PKG) ulexing.cma pa_ulex.cma  pr_o.cmo \
               -sep     '        '\
               -no_ss   \
               -l 80    \
               -impl scanner.mul -o scanner.ml

    
#
#
#
#####################################################################################       













