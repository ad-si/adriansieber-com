main.pdf: main.md
	cat $< \
	| sed 's/```purescript/```haskell/g' \
	| pandoc \
		--highlight-style breezeDark \
		--to beamer \
		--output $@
