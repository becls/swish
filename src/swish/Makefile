SCHEME ?= scheme
MACHINE_TYPE := $(shell echo '(machine-type)' | "${SCHEME}" -q)
BASE_MACHINE_TYPE := $(patsubst t%,%,${MACHINE_TYPE})
CHEZ_VERSION := $(shell "${SCHEME}" --version 2>&1)
ifneq (,$(findstring nt,${MACHINE_TYPE}))
EXESUFFIX := .exe
endif
SWISH_VERSION := $(shell "${SCHEME}" --script ../get-software-info short-version)
LIBDIRS := .::../lib

ready: io-constants.ss clibs

all: ready  ../../bin/swish${EXESUFFIX} ../../bin/swish.boot ../../bin/swish.library ../../lib/swish/mat.so ../../lib/swish/profile.so ../../lib/swish/testing.so

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
	'(reset-handler abort)' \
	'(make-boot-file "$@" (quote ("scheme" "petite")) "../../bin/swish.library" "boot.ss")' \
	| ${SCHEME} -q ../../bin/swish.library

../../bin/swish.library: *.ss
	find ../../lib -name '*.so' -delete
	@echo \
	'(waiter-prompt-string "")' \
	'(reset-handler abort)' \
	'(generate-wpo-files #t)' \
	'(compile-imported-libraries #t)' \
	'(cd "..")'\
	'(import (swish imports))' \
	'(compile-whole-library "../lib/swish/imports.wpo" "../bin/swish.library")' \
	| ${SCHEME} -q --libdirs "${LIBDIRS}"

../../lib/swish/%.so: %.ss ../../bin/swish${EXESUFFIX} ../../bin/swish.library
	@echo \
	'(waiter-prompt-string "")' \
	'(reset-handler abort)' \
	'(generate-wpo-files #t)' \
	'(compile-imported-libraries #t)' \
	'(cd "..")'\
	'(library-directories "${LIBDIRS}")' \
	'(import (swish $(<:.ss=)))' \
	| ../../bin/swish${EXESUFFIX} -q

clean: platform-clean
	rm -f io-constants.ss
	rm -f ../*.ss.html ../mat-report.html ../server-profile.html
	rm -f *.mo *.ms.html *.ss.html

INSTLIBDIR := ${INSTALLROOT}/lib/swish${SWISH_VERSION}/${MACHINE_TYPE}
INSTWEBDIR := ${INSTALLROOT}/lib/swish${SWISH_VERSION}/web
INSTDOCDIR := ${INSTALLROOT}/share/doc/swish${SWISH_VERSION}
install: all
ifeq (,${INSTALLROOT})
	$(error INSTALLROOT is not set)
else
	install --directory ${INSTDOCDIR}
	install ../../doc/swish.pdf ${INSTDOCDIR}
	install --directory ${INSTLIBDIR}
	install ../../bin/swish ${INSTLIBDIR}
	install ../../bin/swish-build ${INSTLIBDIR}
	install ../../bin/swish.boot ${INSTLIBDIR}
	install ../../bin/swish.library ${INSTLIBDIR}
	install ${SwishLibs} ${INSTLIBDIR}
	install --directory ${INSTALLROOT}/bin
	ln -sf ${INSTLIBDIR}/swish${EXESUFFIX} ${INSTALLROOT}/bin/swish
	ln -sf ${INSTLIBDIR}/swish-build ${INSTALLROOT}/bin/swish-build
	install --directory ${INSTLIBDIR}/lib
	cd ../../lib ; find . -type f -name '*.wpo' -exec install -D -T {} ${INSTLIBDIR}/wpo/{} \;
	cd ../../lib ; find . -type f -name '*.so' -exec install -D -T {} ${INSTLIBDIR}/lib/{} \;
	install ../../lib/swish/mat.so ${INSTLIBDIR}/lib/swish
	install ../../lib/swish/profile.so ${INSTLIBDIR}/lib/swish
	install ../../lib/swish/testing.so ${INSTLIBDIR}/lib/swish
	install --directory ${INSTWEBDIR}
	cd ../../web ; find . -type f -exec install -D -T {} ${INSTWEBDIR}/{} \;
endif
