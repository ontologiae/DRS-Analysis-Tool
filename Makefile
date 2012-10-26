TARGET=drsxp

SOURCES = \
	syntax.ml \
	lexer.mll \
	parser.mly \
	paraphraser.ml \

OCAMLBUILD=ocamlbuild 
#-use-menhir
CAML2HTML=caml2html
OCAMLDOC=ocamldoc

default: byte

all: byte native html

top: 
	utop -I _build/ parser.cmo message.cmo lexer.cmo syntax.cmo drsxp.cmo -init paraphraser.ml

stop: clean byte
	utop -I _build/ parser.cmo message.cmo lexer.cmo syntax.cmo drsxp.cmo -init top.ml 


byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

web: html
	echo '<div class="lang">' > web.html
	echo "<h3>$(TARGET)</h3>" >> web.html
	echo '<div class="version">Last update: ' >> web.html
	echo '</div>' >> web.html
	echo '<div class="description">' >> web.html
	cat description.html >> web.html
	echo '</div>' >> web.html
	echo '<div class="download">Download source: <a href="src/$(TARGET).zip">$(TARGET).zip</a></div>' >> web.html
	/bin/echo -n '<div class="source"><a href="html/$(TARGET).html">View source online</a> (' >> web.html
	cat $(SOURCES) | wc -l >> web.html
	echo ' lines)</div>' >> web.html
	echo "</div>" >> web.html

html:
	/bin/mkdir html
	$(CAML2HTML) -nf -ln -noannot -o html/$(TARGET).html $(SOURCES)

clean:
	/bin/rm -rf web.html html/
	$(OCAMLBUILD) -clean
