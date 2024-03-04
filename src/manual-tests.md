# Manual Tests

## Signal Handling

### Windows

Please see the `run-manual-test` script.

### Unix-like Systems

Signal-handling tests are automated on Unix-like systems.
See [app.ms](swish/app.ms) and [io.ms](swish/io.ms).

## Keyboard Interrupt

Here are some test cases to try at the REPL.
These are also captured in the [`expect` scripts](../test/ReadMe.md).

### Simple Case

 1. start the Swish REPL
 2. press ^C; this has no effect: we simply get another REPL prompt
 3. run `(let f () (f))`
 4. press ^C; this should interrupt the loop, giving us the "break>" prompt
 5. enter `?` for options

### A More Involved Case

 1. start the Swish REPL
 2. press ^C several times; this just gives us another REPL prompt
 3. run `(let f () (f))`
 4. press ^C to interrupt the loop
 5. at the "break>" prompt, enter `n` to start a new cafe; the prompt changes to ">>" to reflect the cafe nesting level
 6. run `(let g () (g))`
 7. press ^C to interrupt the loop
 8. at the "break>" prompt, enter `e` to exit the interrupt handler and resume the loop
 9. press ^C to interrupt the loop again
 10. at the "break>" prompt, enter `q` to reset to the cafe; again the prompt is ">>" to reflect the current cafe level
 11. run `(new-cafe)`
 12. press ^C a few times; this just gives another REPL prompt
 13. run `(let f () (f))`
 14. press ^C to interrupt the loop
 15. at the "break>" prompt, enter `n` to start a new cafe; the prompt changes to ">>>>" to reflect the cafe nesting level
 16. evaluate `(let f ([n 3]) (if (= n 0) (begin (break) 'ok) (list (f (- n 1)))))`
 17. at the "break>" prompt, enter `n` to start a new cafe; the prompt changes to ">>>>>" to reflect the cafe nesting level
 18. run `(let f () (f))`
 19. press ^C to interrupt the loop
 20. at the "break>" prompt, enter `e` to exit the interrupt handler and resume the loop
 21. press ^C to interrupt the loop again
 22. at the "break>" prompt, press ^C; this just gives us another "break>" prompt
 23. at the "break>" prompt, enter `q` to reset to the cafe; again the prompt is ">>>>>" to reflect the current cafe level
 24. run `(exit)`, returning to the "break>" prompt
 25. at the "break>" prompt, enter `e` to exit and continue the loop from step 16; the REPL prints `(((ok)))`
 26. run `(exit)`, returning to the "break>" prompt
 27. at the "break>" prompt, press ^C; this just gives us another "break>" prompt
 28. at the "break>" prompt, enter `q`, returning to the cafe with prompt ">>>"
 29. run `(let f () (receive (after 5000 (f))))`
 30. press ^C to interrupt the loop
 31. at the "break>" prompt, press ^C a few times; this just gives another "break>" prompt
 32. at the "break>" prompt, enter `q`
 33. run `(exit)`, returning to the cafe with prompt ">>"
 34. run `(let f () (f))`
 35. press ^C to interrupt the loop
 36. at the "break>" prompt, enter `q`
 37. run `(exit)`, returning to the "break>" prompt from step 5
 38. at the "break>" prompt, enter `e` to continue the loop from step 3
 39. press ^C to interrupt the loop
 40. at the "break>" prompt, enter `q`, returning to the ">" prompt
 41. type the incomplete expression `(cons 1` and press ^C; we get a new ">" prompt
 42. run `(* 6 7)`; we get the expected answer
