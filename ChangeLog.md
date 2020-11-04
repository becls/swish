# Change Log

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

