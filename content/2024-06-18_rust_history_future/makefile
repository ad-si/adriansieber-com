.PHONY: build
build: presentation.typ
	typst compile $<


.PHONY: start
start: presentation.typ
	watchexec \
		--exts typ \
		'typst compile $< && reload Safari'
