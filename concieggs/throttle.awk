BEGIN { now=systime(); lines = 0 }
{
  if (lines++ > 3) {
    if (now == systime()) {
      system("sleep 2")
    }
    now = systime()
    lines = 1
  }
  print
  fflush()
}
