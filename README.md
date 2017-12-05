# Swish Concurrency Engine

The Swish Concurrency Engine is a framework used to write
fault-tolerant programs with message-passing concurrency. It uses the
Chez Scheme programming language and embeds concepts from the Erlang
programming language. Swish also provides a web server.

# Design

Swish uses [libuv](http://libuv.org) for cross-platform asynchronous
I/O.

# Notes

- Disable the expression editor with --eedisable because Chez Scheme's
  expression editor does not use asynchronous console I/O. We would
  have to modify the places where s/expeditor.ss calls `$ee-read-char`
  in blocking mode to use libuv's asynchronous read function instead
  of the one in c/expeditor.c.

# Building

## Linux

- Chez Scheme 9.5 for a6le

## Mac

- Chez Scheme 9.5 for a6osx

## Windows

- Chez Scheme 9.5 for a6nt

# Compiling libuv 1.18.0

```bash
$ git submodule update --init libuv
$ cd libuv
$ git clone https://chromium.googlesource.com/external/gyp build/gyp
```

## Linux

```bash
$ ./gyp_uv.py -Duv_library=static_library -f make
$ BUILDTYPE=Release CFLAGS="-fPIC" make -C out
```

## Mac

```bash
$ ./gyp_uv.py -Duv_library=static_library -f xcode
$ xcodebuild -ARCHS="x86_64" -project uv.xcodeproj -configuration Release -target All
```
## Windows

- Install Python 2.7 for Windows.

```cmd
> set PYTHON=C:\Python27\python.exe
> vcbuild.bat release vs2017 x64 static
```
