# Address Sanitizer

- Address Sanitizer is a memory error detector for C/C++ documented
  [here](https://github.com/google/sanitizers/wiki/AddressSanitizer).
- Address Sanitizer is now built into both LLVM and GCC.
- Often referred to as ASAN.
- Since ASAN is faster than Valgrind we might use it more often.

## Caveats

- Address Sanitizer does not work under `ptrace`, (GDB, `strace`, etc.).
  Under GDB, for example, we get exit 1 and a log entry saying that
  LeakSanitizer encountered a fatal error.
- Address Sanitizer doesn't handle `S_call_help`; see instructions below
  showing how to add an exclusion for this.
- Address Sanitizer reports leaks to `stderr` by default, which breaks
  some mats. The instructions below show how to change this.
- We get no ASAN output if we go through the `_exit` path in `osi_exit`
  or `swish_run`. One way this can happen is that `uv_close_loop`
  returns `UV_EBUSY` if we have open handles. For example, in the
  `process-detached` automated test, I forgot to close the
  `tcp-listener` and we still had a handle active when we wanted to exit
  (i.e., when running the test individually).
- By default, ASAN checks at runtime how the application links to the
  library. This check trips when we load the libosi shared library while
  compiling the boot file. We can work around it by setting
  `ASAN_OPTIONS` as described later, but we may be able to set
  `LD_PRELOAD` as another alternative or build Chez Scheme with ASAN
  linked in.
  - Setting `LD_PRELOAD` is problematic. If we set it at the shell, ASAN
    ends up trying to check `make` and aborts due to apparent leaks
    there. We might be able to tweak the Makefile to set `LD_PRELOAD`
    while compiling our boot file.
- Don't build with the static ASAN package; we want the dynamic libasan
  package so we can build our libosi shared library.

## Setup on macOS

1.  May need recent clang from homebrew:
    `export PATH=/usr/local/opt/llvm/bin:$PATH`
2.  May need to explicitly enable leak checking:
    `export ASAN_OPTIONS=detect_leaks=1:verify_asan_link_order=0`
3.  Need to ignore "nano zone abandoned": `export MallocNanoZone=0`
4.  For unknown reasons, we need to compile Chez Scheme with ASAN turned
    on:
    `./configure CC=clang CFLAGS="-fsanitize=address -g -fno-omit-frame-pointer" --installprefix=~/tmp/scout`
5.  For Swish, need to point to custom Chez Scheme (not in /usr to avoid
    System Integrity Protections) and apparently we need ASAN linked to
    Chez Scheme (see step 4):
    `./configure CC=clang CFLAGS="-fsanitize=address -g -fno-omit-frame-pointer" --prefix=~/tmp/scout --scheme=~/src/chez-scheme/a6osx/bin/a6osx/scheme`
6.  Swish's configure will suggest something like this:
    `export SCHEMEHEAPDIRS="~/src/chez-scheme/a6osx/boot/%m:~/src/chez-scheme/a6osx/bin/lib/csv%v/%m"`

## Setup on Linux

ASAN likely works out of the box with LLVM and GCC.

- Fedora: if necessary, try `dnf install libasan-static`.
- Ubuntu: if necessary, try `apt install libasan6`.

## Build and run Swish with ASAN

1.  ASAN needs a lot of virtual memory. Check that `ulimit -v` is not
    set. You may need to edit `~/.bashrc` and start a new shell to
    remove the limit.

2.  build using dynamic Address Sanitizer:

    ``` bash
    $ ./configure CFLAGS="-fsanitize=address -g -fno-omit-frame-pointer"
    # we need the following while building the boot file
    $ export ASAN_OPTIONS="verify_asan_link_order=0"
    $ make clean
    $ make -C src/swish mat-prereq
    ```

3.  set environment variables and prepare filesystem:

    ``` bash
    # ignore the "leak" due to setjmp/longjmp in S_call_help:
    $ echo "leak:S_call_help" > ignore-S_call_help
    $ export LSAN_OPTIONS=suppressions="${PWD}/ignore-S_call_help"
    # redirect output so mats that check stderr will pass and
    # disable verify_asan_link_order for the same of repl.ms
    # and other mats that invoke scheme-exe:
    $ mkdir -p /tmp/asan-logs
    $ export ASAN_OPTIONS=log_path=/tmp/asan-logs/log:detect_leaks=1:verify_asan_link_order=0
    # enable more checks, if desired:
    $ export ASAN_OPTIONS="${ASAN_OPTIONS}:check_initialization_order=true detect_stack_use_after_return=true strict_string_checks=true"
    ```

4.  run `make test` or `cd src; ./run-suite swish/io.ms` for example

5.  check the files written to the /tmp/asan-logs `log_path`;

    - expect to see /tmp/asan-logs/log.\<pid\> files containing:

      ``` example
      -----------------------------------------------------
      Suppressions used:
        count      bytes template
            1        200 S_call_help
      -----------------------------------------------------
      ```

    - a quick way to find unexpected logs is

      ``` bash
      $ sha1sum /tmp/asan-logs/* | grep -v 06daefdb280f66d645dd06272ad3edce254d9617
      ```

## Tips

- To identify where we're leaking in the mats, it helps to patch
  `run-os-process` in swish/testing.ss by adding the following just
  after `(write-stdin to-stdin)`; then we can correlate the log.\<pid\>
  file showing a leak with the cmdline.\<pid\> file showing what we fed
  to `run-os-process`.

  ``` scheme
  (call-with-output-file (format "/tmp/asan-logs/cmdline.~s" os-pid) write-stdin)
  ```

- To iterate on checking leaks in a suite:

  ``` bash
  # $1 is suite name, e.g., swish/foreign.ms
  function foo {
    rm /tmp/asan-logs/* ;
    ./run-suite $1;
    sha1sum /tmp/asan-logs/* | grep -v 06daefdb280f66d645dd06272ad3edce254d9617 ;
  }
  # $1 is suite name, $2 is test name
  # (won't work with erlang.ms etc. where we need access to internal library)
  function bar {
    rm /tmp/asan-logs/* ;
    ./run-mat -b $1 $2;
    sha1sum /tmp/asan-logs/* | grep -v 06daefdb280f66d645dd06272ad3edce254d9617 ;
  }
  ```

# Why not Valgrind?

- Valgrind may be more capable. For example it may detect some errors in
  Scheme code.
- However, Valgrind is slower than ASAN.
- libuv 1.45.0 introduced `io_uring` support
  - this improved linux file I/O performance
  - but we now hit this [valgrind false positive
    issue](https://github.com/libuv/libuv/issues/4008)
- Hopefully we will be able use either tool in the future.
