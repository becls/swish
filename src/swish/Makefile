SCHEME ?= scheme
MACHINE_TYPE := $(shell echo '(machine-type)' | "${SCHEME}" -q)
BASE_MACHINE_TYPE := $(patsubst t%,%,${MACHINE_TYPE})
CHEZ_VERSION := $(shell "${SCHEME}" --version 2>&1)
ifneq (,$(findstring nt,${MACHINE_TYPE}))
EXESUFFIX := .exe
endif
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
