program sum
  implicit none
  integer i
  real :: x
  real :: y
  character(len=32) :: arg ! Hvilket tal er længere end det her? Det
                           ! tror jeg ikke der er noget tal der er.

  y = 0
  i = 1
  do
     call get_command_argument(i, arg)
     if (LEN_TRIM(arg) == 0) EXIT

     read(arg,*) x
     y = y + x
     i = i + 1
  end do
  print *, y
end program sum
