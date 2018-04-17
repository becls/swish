all: swish doc/swish.pdf

doc/swish.pdf: doc/*.sty doc/*.bib doc/swish/*.tex
	make -C doc

swish:
	make -C src/swish all

test: swish
	cd src; ./run-mats ${PWD}/bin
	@echo "see file://${PWD}/src/mat-report.html"

coverage:
	cd src; PROFILE_MATS=yes ./run-mats
	@echo "see file://${PWD}/src/server-profile.html"

destknown:
ifeq (,${INSTALLROOT})
	$(error INSTALLROOT is not set)
endif

install: destknown all
	make -C src/swish install

clean:
	make -C doc clean
	make -C src/swish clean
