# Swish Concurrency Engine

The Swish Concurrency Engine is a framework used to write
fault-tolerant programs with message-passing concurrency. It uses the
Chez Scheme programming language and embeds concepts from the Erlang
programming language. Swish also provides a web server.

# [Design](https://becls.github.io/swish/swish.pdf)

Swish uses [libuv](http://libuv.org) for cross-platform asynchronous
I/O.

# Notes

1. install the prerequisites (see Build System Requirements)
2. `./configure` (see `./configure --help` for options)
3. `make`
4. `make test`

- After `./configure`; you can also `cd src; ./go` to build and run the engine.
- We disable the expression editor with --eedisable because Chez Scheme's
  expression editor does not use asynchronous console I/O. We would
  have to modify the places where s/expeditor.ss calls `$ee-read-char`
  in blocking mode to use libuv's asynchronous read function instead
  of the one in c/expeditor.c.

# Build System Requirements

## Linux

- Chez Scheme 9.5.1 from July 18, 2018 or later (commit a0adfa1, which adds load-compiled-from-port)
- GCC, the GNU Compiler Collection
- GNU C++ compiler for libuv
- GNU make
- Python 2.7 for libuv
- uuid-dev package

## Mac

- Chez Scheme 9.5.1 from July 18, 2018 or later (commit a0adfa1, which adds load-compiled-from-port)
- dot (can be installed through homebrew using `brew install graphviz --with-app`)
- pdflatex (can be installed through homebrew using `brew cask install mactex`)
- realpath (can be installed through homebrew using `brew install coreutils`)
- XCode and the command-line tools must be set (Xcode->Preferences->Locations->command line tools)

## Windows

- Chez Scheme 9.5.1 from July 18, 2018 or later (commit a0adfa1, which adds load-compiled-from-port)
- Cygwin with bash, git, graphviz, grep, perl, texlive, GNU make, etc.
- Microsoft Visual Studio 2017 or 2015 with Visual C++
- Python 2.7 for Windows in C:\Python27 (see below for other options)
- Put scheme in PATH.

### Building on Windows with Miniconda Python

- Install [Python 2.7](https://conda.io/miniconda.html)
- Provide the path to the python2.7 executable as an argument to the configure script:
  ```
  ./configure --python=~/Miniconda2/python.exe
  ```

### Building on Windows with Anaconda Python

- Make an [environment containing Python 2.7](https://conda.io/docs/user-guide/tasks/manage-python.html#installing-a-different-version-of-python)

  `conda create -n py27 python=2.7 anaconda`

- Provide the path to the python2.7 executable as an argument to the configure script:
  ```
  ./configure --python=~/Anaconda3/envs/py27/python.exe
  ```
