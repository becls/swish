# Change Log

## 2024-03-12 Version 2.6.0 (Stable)

### New

* Enabled use with Chez Scheme 10.0
* Enabled use with arm64osx, tarm64osx, arm64le, and tarm64le machine types
* Added `db:start`
* Added end timestamp to the child view
* Added `not-reached` macro which omits source when profiling
* Added `port->notify-port` for TCP ports
* Added `tcp-nodelay`
* WebSockets
  - Added error code 1009 when exceeding size limit
  - Added error code 1011 when reader raises an exception
  - Improved performance by enabling `tcp-nodelay`
  - Improved performance by masking in place rather than copying the
    payload using optimized primitives
  - Improved performance by reusing the codec buffer if possible
  - Improved performance by sending the header and payload with a
    single call `osi_tcp_write2`
* Added `get-bytevector-exactly-n`
* Added `define-syntactic-monad`
* Added `natural-string<?` and `natural-string-ci<?`
* Added `osi_is_service`
* Added `fold-files` and `filter-files`
* Added `--prelude` and `--prelude-file` to `swish-build`
* Added `swish-test --progress suite-verbose`
* Improved performance of JSON reading and writing
* Added `json:pretty`, `json:key<?`, and `json:custom-write`
* Added parallel library including `parallel`, `parallel!`,
  `parallel:execute`, `parallel:execute!`, `parallel:for-each`,
  `parallel:map`, and `parallel:vector-map`
* Added support for procedural `process-trap-exit` handler

### Bug Fixes

* Fixed writing to closed stderr on exception
* Fixed clearing unset signal handler
* Clarified scopes for define-options sub-forms
* HTTP and WebSocket code now closes ports rather than leaving them to
  the collector
* Fixed console-event-handler retaining objects indefinitely when
  attempting to suppress duplicate stack dumps
* Fixed leak in sqlite:marshal-bindings where an out-of-range value
  may cause the Scheme runtime to longjmp beyond Swish's error handling
* Fixed premature closing of WebSocket while reading large payloads
* Fixed usage of `get-bytevector-n`
* Fixed to release profile counts using `profile-release-counter`
  introduced in Chez Scheme 9.5.1
* Fixed destination for annotated files when profiling
* Fixed `swish-build -s` to resolve `<source>` files using the
  `source-directories` specified
* Fixed potential `watch-path` crash
* Fixed typical use-cases for Swish scripts under Cygwin

### Updates

* Updated to SQLite 3.45.1
* Updated to libuv 1.48.0
* Require Chez Scheme 9.6.4 or later
* Eliminated dependency on jQuery
* Updated profile coverage report
  - added legend
  - added line numbers
  - changed DOCTYPE to HTML5
  - no longer colors leading whitespace
* Updated `html5` form to include `lang="en"` if not otherwise specified
* Updated `console-event-handler` to elide specific `<child-end>` messages

## 2022-06-09 Version 2.4.0 (Stable)

### New

* Breaking change: swish-test no longer adds an automatic .ms suffix
  when checking whether a specified file exists.
* Breaking change: swish-test exits with a non-zero exit status when
  processing results that are incomplete or contain failed tests,
  skipped tests, or no passing tests. The aim is for `--harvest` or
  `--load-results` runs to generate a non-zero exit status when the
  original test run that produced those results would have done so.
* Improved performance of mon and pcb records in hashtables by
  implementing custom record-type-hash-procedure for each.
* Added `os-pid`, `current-memory-bytes`, `maximum-memory-bytes`,
  `os-free-memory`, and `os-total-memory` to statistics log.
* Added `osi_get_home_directory` to support limited tilde expansion
  for filesystem paths.
* Added `default-timeout` and `scale-timeout` to testing, the latter
  respecting an optional `TIMEOUT_SCALE_FACTOR` environment variable.
* Added `LD` compile option to configure script.
* swish-test:
  - Preserves stdout and stderr output
  - Preserves `--harvest` suite order
  - Added `--rerun` option
  - Added "Download Data" link to HTML test report
* Changed `throw` to `raise` for `http-request-timeout` errors.

### Bug Fixes

* mat load-results now preserves result order

### Updates

* Updated to SQLite 3.38.5
* Updated to libuv 1.44.1
* Require Chez Scheme 9.5.6 or later

## 2021-05-18 Version 2.2.0 (Stable)

### New

* Improved database log performance:
  - Added `osi_bulk_execute` and `sqlite:bulk-execute` to support faster bulk
    insert.
  - Changed `db` gen-server to flush the accumulated `db:log` entries via
    `sqlite:bulk-execute` for better performance.
  - Added `sqlite:marshal-bindings` and `sqlite:unmarshal-bindings`.
  - Added `osi_get_bindings` and `sqlite:get-bindings`.
  - Extended `sqlite:execute` to accept as bindings: a list, a vector, or a
    marshaled bindings record instance.
* Provided more control over database tuning parameters:
  - Exposed `create-prune-on-insert-trigger`.
  - Added `make-swish-event-logger` to let applications tune prune triggers for
    the Swish event tables.
  - Added `db:options` argument to `db:start&link` and `log-db:start&link`,
    which allows tuning:
    - `commit-limit`
    - `commit-delay`
    - `cache-timeout`
  - Added `db:expire-cache`.
* `osi_interrupt_database` and `sqlite:interrupt` now return a boolean
  indicating the database's busy state.
* Added `log-db:event-logger` that supports the `setup` and `log` fields of
  `<event-logger>`, but also supports a `tolerate-fault?` predicate.
* Added an optional `endure?` argument to `event-mgr:set-log-handler` to let
  applications respond flexibly to log-handler faults.
* Added `define-tuple` support for `(type is?)` that expands into
  `(lambda (x) (type is? x))` just as `(type field)` expands into
  `(lambda (x) (type field x))`.
* Added `ct:join` and `ct:string-append`, which do their work at expand time
  if possible.
* Added to the meta-data collected by automated tests:
  - data from `get-uname`
  - start and end timestamp for each test
  - user annotations logged via `mat:add-annotation!`
* Added `--toolprefix` option to `./configure` to simplify cross-compilation.

### Bug Fixes

* Fixed a websocket bug that reported unexpected end-of-file when a 64-bit
  payload length crosses an internal buffer boundary.
* Improved database gen-server's fault tolerance by using `monitor`
  in place of `link` and by adapting `supervisor` protocol on shutdown.
* Fixed a minor performance bug that made some hashtables in the `http`
  library resort to linear search.
* Fixed an obscure bug in `receive` when Swish is compiled with `cp0` disabled.
  Normal builds leave `cp0` enabled, but `make coverage` disables `cp0`.

### Updates

* Updated to SQLite 3.34.1
* Updated to libuv 1.41.0

## 2021-03-08 Version 2.0.2 (Stable)

### Bug Fixes

Fixed a performance bug. When installing the values of inherited parameters,
`spawn` and `spawn&link` called parameter filters, if any, when they should not
have, since those values were already filtered when the parameters were set.
This was particularly significant for the inherited `app:path` parameter, where
the filter makes an expensive call to `get-real-path`.

## 2020-11-09 Version 2.0.1 (Stable)

### Bug Fixes

Fixed a bug in `$receive` that could cause a process to wait for the full
`waketime`, which could be infinite, for a message that is sent _just_ after
`$receive` has picked a queue to sleep on.

This manifested as strange behavior where a `gen-server:call` to a trivial
gen-server could hang indefinitely if the timeout was specified as `infinity` or
could receive the message just as the call would have timed out, despite the
gen-server replying immediately.

The change in 2f16c0f2f2c1246193368680682a68cc6dfcae46 allowed a sequence of events
where:
1. one process calls `$receive`
2. no message is ready, so we enable interrupts
3. we set pcb fields depending on waketime, then call `yield` with a destination queue
4. `yield` does an interrupt check on entry and we happen to context switch
5. `@send` has a message for the original process and places that process on the run-queue
6. we resume in `yield` and, depending on the specified destination queue, either dequeue the process or put it on the sleep queue

## 2020-10-01 Version 2.0.0 (Stable)

Release numbering skipped 1.x to avoid confusion with internal project.

