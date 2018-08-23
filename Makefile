MAKEFLAGS += --no-print-directory
.PHONY: all clean coverage doc install pristine swish test

all: swish doc

doc:
	$(MAKE) -C doc

swish: src/swish/Makefile
	$(MAKE) -C src/swish all

src/swish/Makefile:
	@echo "Run ./configure to create $@"
	@exit 1

test:
	@$(MAKE) -C src/swish mat-prereq
	@./src/run-mats

coverage:
	@PROFILE_MATS=yes $(MAKE) -C src/swish mat-prereq
	@PROFILE_MATS=yes ./src/run-mats

install: all
	$(MAKE) -C src/swish install

clean: src/swish/Makefile
	$(MAKE) -C doc clean
	$(MAKE) -C src/swish clean

pristine: clean
	rm -f src/swish/Makefile
