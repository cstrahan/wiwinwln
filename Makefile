PANDOC = pandoc
FLAGS = --standalone --toc --toc-depth=2 --highlight-style pygments
TEMPLATE = page.tmpl
STYLE = css/style.css
SRC = tutorial

all: $(SRC).html $(SRC).epub $(SRC).pdf

includes: includes.hs
	ghc --make $<

%.html: %.md includes
	$(PANDOC) -f markdown -t json < $< \
	| ./includes \
	| pandoc -f json -t html --template $(TEMPLATE) -c $(STYLE) $(FLAGS) -o $@

%.epub: %.md includes
	$(PANDOC) -f markdown -t json < $< \
	| ./includes \
	| pandoc -f json -t epub --template $(TEMPLATE) -c $(STYLE) $(FLAGS) -o $@

%.pdf: %.md includes
	$(PANDOC) -f markdown -t json < $< \
	| ./includes \
	| pandoc -f json -t latex --pdf-engine=xelatex -c $(STYLE) $(FLAGS) -o $@

clean:
	rm -f *.html *.epub *.pdf
