# Swish Concurrency Engine

The Swish Concurrency Engine is a framework used to write
fault-tolerant programs with message-passing concurrency. It uses the
Chez Scheme programming language and embeds concepts from the Erlang
programming language. Swish also provides a web server.

# Design

Swish uses [libuv](http://libuv.org) for cross-platform asynchronous
I/O.

# Notes

- `cd src; ./go` builds and runs the engine.
- Disable the expression editor with --eedisable because Chez Scheme's
  expression editor does not use asynchronous console I/O. We would
  have to modify the places where s/expeditor.ss calls `$ee-read-char`
  in blocking mode to use libuv's asynchronous read function instead
  of the one in c/expeditor.c.

# Build System Requirements

## Linux

- Chez Scheme 9.5
- GCC, the GNU Compiler Collection
- GNU C++ compilter for libuv
- Python 2.7 for libuv
- uuid-dev package

## Mac

- Chez Scheme 9.5

## Windows

- Chez Scheme 9.5
- Cygwin with bash, git, graphviz, grep, perl, texlive, etc.
- Python 2.7 for Windows in C:\Python27
- Put scheme in PATH.
