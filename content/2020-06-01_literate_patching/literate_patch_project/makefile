.PHONY: main
main: output


output: 0.patch
	-mkdir output
	git apply \
		--directory=$@ \
		--recount \
		$<


output.pdf: output.md
	pandoc $< \
		--output $@


output.html: output.md
	pandoc $< \
		--standalone \
		--self-contained \
		--output $@


.INTERMEDIATE: output.md
output.md: 0.patch
	printf -- '---\n' > $@
	printf -- 'title: Version $(subst .patch,,$<)\n' >> $@
	printf -- '---\n' >> $@

	gsed \
		-e 's/^--- /```diff\n--- /g' \
		-e 's/^-----$$/```\n-----/g' \
		-e '$$a```' \
		-e 's/^Subject:/\n/g' \
		-e 's/^From:/By/g' \
		$< \
		>> $@


.PHONY: clean
clean:
	-rm -rf output
	-rm output.md
	-rm output.pdf
