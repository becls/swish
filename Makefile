MAKEFLAGS += --no-print-directory
.PHONY: clean coverage check-docs doc install pristine swish test safe-check

swish: src/swish/Makefile
	$(MAKE) -C src/swish all

doc:
	$(MAKE) -C doc

src/swish/Makefile:
	@echo "Run ./configure to create $@"
	@exit 1

test: src/swish/Makefile
	@$(MAKE) -C src/swish mat-prereq
	@./src/run-mats

safe-check: src/swish/Makefile
	@if git grep -l '[(]include "[^"]\+unsafe.ss"[)]'; then echo "inconsistent include of unsafe.ss in file(s) listed above"; exit 1; fi
	@touch src/swish/unsafe.ss
	@UNSAFE_PRIMITIVES=no $(MAKE) -C src/swish mat-prereq
	@./src/run-mats

coverage: src/swish/Makefile
	@PROFILE_MATS=yes $(MAKE) -C src/swish mat-prereq
	@PROFILE_MATS=yes ./src/run-mats

check-astyle:
	@(astyle --project $$(git ls-files '*.c' '*.h' | grep -v 'sqlite3'))

check-docs: src/swish/Makefile
	@(cd src; ./go check-docs -uD -e '^osi_.*\*' -e '^[A-Z_]+' -e '^\$$[a-z-]+' -e '^event-mgr:unregister' ..)

warn-letrec-check:
	@WARN_UNDEFINED=yes $(MAKE) -C src/swish all

install: swish doc
	$(MAKE) -C src/swish install install-batteries

clean: src/swish/Makefile
	$(MAKE) -C doc clean
	$(MAKE) -C src/swish clean

pristine: clean
	$(MAKE) -C src/swish pristine
	rm -f src/swish/Makefile
	rm -f src/swish/Mf-config
	rm -f src/swish/sh-config
	rm -f src/osi-bootstrap.ss
