purescript/output/Add/index.js: purescript/src/Add.purs
	purs compile add.purs

.PHONY: test
test:
	cd gleam && gleam test
	cd purescript && spago test
	cd elm && elm make ./src/Main.elm
	cd vlang && v -o add.js add.v
	# cd lean && lean --c=Add.c Add.lean
