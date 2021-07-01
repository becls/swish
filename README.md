[![Build Status](https://github.com/becls/swish/actions/workflows/test.yml/badge.svg?branch=dev)](https://github.com/becls/swish/actions/workflows/test.yml)

# Swish Concurrency Engine

The Swish Concurrency Engine is a framework used to write
fault-tolerant programs with message-passing concurrency. It uses the
[Chez Scheme](https://cisco.github.io/ChezScheme) programming language
and embeds concepts from the Erlang programming language. Swish also
provides a web server.

# Design

The latest design document can be found
[here](https://becls.github.io/swish/swish.pdf).

Swish uses [libuv](http://libuv.org) for cross-platform asynchronous
I/O.

Although Swish can be run in threaded Chez Scheme for convenience, it
is not thread safe and should be used from the main thread only.

# Versioning

Swish follows the [semantic versioning](http://semver.org/)
scheme, starting with 2.0.0 to avoid confusion with internal projects.

Odd minor-version numbers are used for development. As such,
we are extremely unlikely to use a patch number other than 0 during
development.

Even minor-version numbers are used for official tagged
releases.

The first commit after an official tagged release should increment
either the minor-version or patch-version number so that the version
number for an official tagged release corresponds to a unique commit
hash.

# Notes

1. install the prerequisites (see Build System Requirements)
1. `./configure` (see `./configure --help` for options)
1. `make`
1. `make test`

- After `./configure`; you can also `cd src; ./go` to build and run the engine.
- We disable the expression editor with --eedisable because Chez Scheme's
  expression editor does not use asynchronous console I/O. We would
  have to modify the places where s/expeditor.ss calls `$ee-read-char`
  in blocking mode to use libuv's asynchronous read function instead
  of the one in c/expeditor.c.
- If you get a "symbol(s) not found" error, you may need to use CPPFLAGS
  and LDFLAGS to supply the header and library path. If the C compiler
  refuses unused arguments, you may need
  `CFLAGS="-Qunused-arguments"`. e.g.,

  ```
  ./configure CPPFLAGS="-I/usr/local/opt/libiconv/include" \
    CFLAGS="-Qunused-arguments" LDFLAGS="-L/usr/local/opt/libiconv/lib"
  ```

# Build System Requirements

## Linux

- Chez Scheme 9.5.4 or 9.5.5
- GCC, the GNU Compiler Collection
- GNU make
- GNU C++ compiler for libuv
- cmake for libuv
- libsystemd-dev and uuid-dev packages
- graphviz, texlive, texlive-latex-recommended, and texlive-latex-extra packages for building the documentation

## Mac

- Chez Scheme 9.5.4 or 9.5.5
- ginstall and realpath (can be installed through homebrew using `brew install coreutils`)
- cmake for libuv (can be installed through homebrew using `brew install cmake`)
- XCode Command Line Tools
- dot (can be installed through homebrew using `brew install graphviz`)
- pdflatex (can be installed through homebrew using `brew cask install mactex`)
- Latin Modern fonts from LaTeX (can be installed with Font Book from a location like `/usr/local/texlive/2020/texmf-dist/fonts/opentype/public/lm`)

## Windows

- Chez Scheme 9.5.4 or 9.5.5
- Cygwin or MinGW/MSYS with bash, git, graphviz, grep, perl, texlive,
  GNU make, etc.
- Microsoft Visual Studio 2019 or 2017 with Visual C++
- Put scheme in PATH.

# Stand-alone Swish Applications

Swish can be used to build, test, and deploy stand-alone
applications. A given application might load foreign code for image
processing or USB access. Code that may block should use the API
described in the "Operating System Interface" chapter of the
[documentation](https://becls.github.io/swish/swish.pdf) to
integrate with Swish's I/O loop.

## Build

For details about building a Swish application, see:
`swish-build --help`

## Test

For details about testing a Swish application, see:
`swish-test --help`

## Deploying a Stand-alone Application

On Linux and macOS, you can deploy your application's executable and
boot file.

On Windows, your install should include the application's executable
and boot file, `osi.dll`, `uv.dll`, `sqlite3.dll`, Chez Scheme's
`csv954.dll`, and Microsoft's C Runtime Library `vcruntime140.dll`.

Developers writing stand-alone applications should clone the Swish
repository and run `configure`.  Swish's source repository provides
`swish.h` to define callable exports for `osi.dll`. `Mf-config` can be
used in makefiles to define variables for system-specific paths.
