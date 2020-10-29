.PHONY: all
all: presentation_dark.pdf presentation_light.pdf


presentation_dark.pdf: presentation.md
	cat $< \
	| pandoc \
		--pdf-engine=xelatex \
		--highlight-style breezeDark \
		--to beamer \
		--output $@


presentation_light.pdf: presentation.md
	cat $< \
	| sed 's/colortheme: owl//g' \
	| pandoc \
		--pdf-engine=xelatex \
		--highlight-style kate \
		--to beamer \
		--output $@
