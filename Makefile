MAKEFLAGS += --no-print-directory
.PHONY: clean coverage doc install pristine swish test

swish: src/swish/Makefile
	$(MAKE) -C src/swish all

doc:
	$(MAKE) -C doc

src/swish/Makefile:
	@echo "Run ./configure to create $@"
	@exit 1

test:
	@$(MAKE) -C src/swish mat-prereq
	@./src/run-mats

coverage:
	@PROFILE_MATS=yes $(MAKE) -C src/swish mat-prereq
	@PROFILE_MATS=yes ./src/run-mats

install: swish doc
	$(MAKE) -C src/swish install

clean: src/swish/Makefile
	$(MAKE) -C doc clean
	$(MAKE) -C src/swish clean

pristine: clean
	rm -f src/swish/Makefile
