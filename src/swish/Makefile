SCHEME ?= scheme
MACHINE_TYPE := $(shell echo '(machine-type)' | "${SCHEME}" -q)
BASE_MACHINE_TYPE := $(patsubst t%,%,${MACHINE_TYPE})
CHEZ_VERSION := $(shell "${SCHEME}" --version 2>&1)
ifneq (,$(findstring nt,${MACHINE_TYPE}))
EXESUFFIX := .exe
endif
SWISH_VERSION := $(shell "${SCHEME}" --script ../get-software-info short-version)

ready: io-constants.ss clibs ../../bin/swish.boot

all: ready ../../bin/swish${EXESUFFIX} ../../bin/swish.library ../../lib/swish/mat.so ../../lib/swish/profile.so ../../lib/swish/testing.so

ifneq (,$(wildcard Mf-${BASE_MACHINE_TYPE}))
include Mf-${BASE_MACHINE_TYPE}
else
$(error Machine type ${MACHINE_TYPE} is not supported)
endif

io-constants.ss: io-constants${EXESUFFIX}
	./$< > $@

../../libuv/include:
	git submodule update --init ../../libuv
	git clone https://chromium.googlesource.com/external/gyp ../../libuv/build/gyp

../../bin/swish.boot: boot.ss ../../bin/swish.library
	@echo \
	'(base-exception-handler (lambda (e) (display-condition e) (newline) (exit 1)))' \
	'(make-boot-file "$@" (quote ("scheme" "petite")) "../../bin/swish.library" "boot.ss")' \
	| ${SCHEME} -q ../../bin/swish.library

../../bin/swish.library: *.ss
	find ../../lib -name '*.so' -delete
	@echo \
	'(waiter-prompt-string "")' \
	'(base-exception-handler (lambda (e) (display-condition e) (newline) (exit 1)))' \
	'(generate-wpo-files #t)' \
	'(compile-imported-libraries #t)' \
	'(cd "..")'\
	'(import (swish imports))' \
	'(compile-whole-library "../lib/swish/imports.wpo" "../bin/swish.library")' \
	| ${SCHEME} -q --libdirs ".::../lib"

../../lib/swish/%.so: %.ss ../../bin/swish ../../bin/swish.library
	@echo \
	'(waiter-prompt-string "")' \
	'(base-exception-handler (lambda (e) (display-condition e) (newline) (exit 1)))' \
	'(generate-wpo-files #t)' \
	'(compile-imported-libraries #t)' \
	'(cd "..")'\
	'(library-directories ".::../lib")' \
	'(import (swish $(<:.ss=)))' \
	| ../../bin/swish

clean: platform-clean
	rm -f ../*.ss.html ../mat-report.html ../server-profile.html
	rm -f *.mo *.ms.html *.ss.html

INSTLIBDIR := ${INSTALLROOT}/lib/swish${SWISH_VERSION}/${MACHINE_TYPE}
INSTWEBDIR := ${INSTALLROOT}/lib/swish${SWISH_VERSION}/web
INSTDOCDIR := ${INSTALLROOT}/share/doc/swish${SWISH_VERSION}
install: all
ifeq (,${INSTALLROOT})
	echo "TBD"
	$(error INSTALLROOT is not set)
else
	install --directory ${INSTDOCDIR}
	install ../../doc/swish.pdf ${INSTDOCDIR}
	install --directory ${INSTLIBDIR}
	install ../../bin/swish{,-build,.boot,.library} ${INSTLIBDIR}
	install ${CLIBS} ${INSTLIBDIR}
	install --directory ${INSTALLROOT}/bin
	ln -sf ${INSTLIBDIR}/swish ${INSTALLROOT}/bin/swish
	ln -sf ${INSTLIBDIR}/swish-build ${INSTALLROOT}/bin/swish-build
	install --directory ${INSTLIBDIR}/lib
	cd ../.. ; find lib -type f -name '*.wpo' -exec install -D -T {} ${INSTLIBDIR}/{} \;
	install ../../lib/swish/mat.so ${INSTLIBDIR}/lib/swish
	install ../../lib/swish/profile.so ${INSTLIBDIR}/lib/swish
	install ../../lib/swish/testing.so ${INSTLIBDIR}/lib/swish
	install --directory ${INSTWEBDIR}
	cd ../../web ; find . -type f -exec install -D -T {} ${INSTWEBDIR}/{} \;
endif
