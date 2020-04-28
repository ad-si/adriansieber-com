.PHONY: all
all: main_dark.pdf main_light.pdf


main_dark.pdf: main.md
	cat $< \
	| sed 's/```elm/```haskell/g' \
	| pandoc \
		--pdf-engine xelatex \
		--highlight-style breezeDark \
		--to beamer \
		--output $@


main_light.pdf: main.md
	cat $< \
	| sed 's/```elm/```haskell/g' \
	| sed 's/colortheme: owl//g' \
	| pandoc \
		--pdf-engine xelatex \
		--highlight-style kate \
		--to beamer \
		--output $@
