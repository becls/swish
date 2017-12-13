(meta-cond
 [(memq (machine-type) '(a6le i3le arm32le))
  (load-shared-object "../bin/libosi.so")]
 [(memq (machine-type) '(a6nt i3nt))
  (load-shared-object "..\\bin\\osi.dll")]
 [(memq (machine-type) '(a6osx i3osx))
  (load-shared-object "../bin/libosi.dylib")]
 [else (error #f "Unsupported machine type")])
