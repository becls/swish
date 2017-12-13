ifeq ($(shell uname -m),armv7l)
ARCH := arm32
else ifeq ($(getconf LONG_BIT),32)
ARCH := i3
else
ARCH := a6
endif
OS := $(shell uname)
ifeq ($(findstring CYGWIN,$(OS)),CYGWIN)
EXESUFFIX := .exe
include Mf-$(ARCH)nt
else ifeq ($(OS),Darwin)
include Mf-$(ARCH)osx
else ifeq ($(OS),Linux)
include Mf-$(ARCH)le
else
$(error Operating system $(OS) is not supported.)
endif

io-constants.ss: io-constants$(EXESUFFIX)
	./$< > $@

../../libuv/include:
	git submodule update --init ../../libuv
	git clone https://chromium.googlesource.com/external/gyp ../../libuv/build/gyp
