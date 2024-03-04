proc finish {r} {
  global sid
  exec kill -9 $sid
  close
  wait
  if { $r == 0} {
    puts "\npass"
  } else {
    puts "\nFAIL"
  }
  exit $r
}

proc pass {} { finish 0 }
proc fail {} { finish 1 }

proc must {pat} {
  expect {
    "$pat" {}
    timeout { puts "\n==> did not get $pat"; fail }
  }
}
