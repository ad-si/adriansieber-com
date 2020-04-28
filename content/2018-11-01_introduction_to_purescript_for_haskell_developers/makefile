.PHONY: all
all: main_dark.pdf main_light.pdf


main_dark.pdf: main.md
	cat $< \
	| sed 's/```purescript/```haskell/g' \
	| pandoc \
		--highlight-style breezeDark \
		--to beamer \
		--output $@


main_light.pdf: main.md
	cat $< \
	| sed 's/```purescript/```haskell/g' \
	| sed 's/colortheme: owl//g' \
	| pandoc \
		--highlight-style kate \
		--to beamer \
		--output $@
