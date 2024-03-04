# Console Tests

The console tests here use [Expect](https://core.tcl.tk/expect/index)
to script interaction with the Swish REPL. The `*.exp` files in this
directory are test cases.

## Running Tests

The scripts here use the version of `swish` that is in your `PATH`
so we usually want to start with:

```
$ make -C ../src/swish mat-prereq
$ export PATH=../build/mat-prereq/lib/swish.x.y.z/arch/:${PATH}
```

To run all of the tests with console interaction logged to a temporary file:
```
$ ./go
```

Be sure to use `grep` to confirm that the Swish revision logged in the temporary
file matches the revision you intended to test. The `go` script prints the path
to the temporary log file as its first line of output.

To run a specific test case:
```
$ expect case3.exp
```

## Writing Tests

Keep the following in mind as you adapt the examples in this directory:

1. Include `source common.tcl` near the top of your script.
   This file provides the `pass`, `fail`, and `must` commands.
   These commands kill the `swish` process when exiting so that it
   does not spin out of control and run afoul of the OOM killer.
2. Use `set sid [spawn swish]` to spawn `swish`. The `pass`, `fail`, and `must`
   commands all rely on the global variable `sid` which must contain the PID of
   the spawned process.
3. End your script by calling `pass`. This kills the spawned `swish`
   application, prints a "pass" message, and returns a suitable exit code.
4. Use `must` when looking for a single pattern. This calls `fail` if we time
   out before finding the pattern.
5. Use `fail` if you want to write a more complicated `expect` case that
   explicitly rejects certain outputs.
6. Patterns are a bit unusual. For example, anchored match `^` and `$` do not
   behave as expected. When matching a cafe prompt, include an explicit newline.
   For example, if you want to prevent ">>" from matching ">>>", use
   `must "\n>> "`.
7. Tcl substitutions apply within `*.exp` files. So instead of writing `send
   "(receive [,x x])\n"`, we write `send {(receive [,x x])}` followed by `send
   "\n"`, using braces to prevent Tcl from trying to evaluate `[,x x]`.
8. To send `^C` use `send \x03`. To send `^D` use `send \x04` and so on.
   Before sending `^C` allow the system enough time or check for explicit
   output such as a prompt to ensure that the program is running what you
   think it is. The `send -h` form may be useful in some cases, but may also
   slow scripts down more than necessary. Matching a prompt can help keep
   things quick.
9. Remember that the console will echo the input we enter via `send`. This can
   lead to subtle bugs.

   For example, `send "(pretty-print 'hello)"` followed by `must "hello"` will
   match the "hello" echoed back as Expect enters the _input_ expression at the
   `swish` REPL. Note that we did not send a newline, so Swish did not evaluate
   the expression in this example, yet we match the output we would naively
   expect.
