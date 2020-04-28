+++
title = "Makefile Cheatsheet"
draft = true
+++

```makefile
path1/tgt1.pdf path2/tgt2.pdf: pre1/requisite1.txt pre2/requisite2.txt | order_only
	@echo '$$@   '  $@
	@echo '$$<   '  $<
	@echo '$$?   '  $?
	@echo '$$^   '  $^
	@echo '$$+   '  $+
	@echo '$$|   '  $|
	@echo '$$*   '  $*
	@echo '$$(@D)'  $(@D)
	@echo '$$(@F)'  $(@F)
	@echo '$$(*D)'  $(*D)
	@echo '$$(*F)'  $(*F)
	@echo '$$(%D)'  $(%D)
	@echo '$$(%F)'  $(%F)
	@echo '$$(<D)'  $(<D)
	@echo '$$(<F)'  $(<F)
	@echo '$$(^D)'  $(^D)
	@echo '$$(^F)'  $(^F)
	@echo '$$(+D)'  $(+D)
	@echo '$$(+F)'  $(+F)
	@echo '$$(?D)'  $(?D)
	@echo '$$(?F)'  $(?F)


pre1/requisite1.txt pre2/requisite2.txt order_only:
	@# Do nothing

.PHONY: path_1/to_1/target_1.pdf path_2/to_2/target_2.pdf
```
