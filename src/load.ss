(meta-cond
 [(memq (machine-type) '(a6le ta6le i3le ti3le arm32le))
  (load-shared-object "../bin/libosi.so")]
 [(memq (machine-type) '(a6nt ta6nt i3nt ti3nt))
  (load-shared-object "..\\bin\\osi.dll")]
 [(memq (machine-type) '(a6osx ta6osx i3osx ti3osx))
  (load-shared-object "../bin/libosi.dylib")]
 [else (error #f "Unsupported machine type")])
