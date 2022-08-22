! RUN: %python %S/test_errors.py %s %flang_fc1
! Check for semantic errors in lcobound() function references
program test_dim_versus_rank
  integer :: array1(2,2,2)
  integer :: a(2,3), b(2,3), c(3,3)
  integer :: i, result_val
  real :: d(2,3)
  logical :: mask(2,3)

  c = reshape( (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))

  c = cshift(c, SHIFT=(/1, 2, -1/), DIM=-1)
  c = cshift(c, SHIFT=(/1, 2, -1/), DIM=0)
  c = cshift(c, SHIFT=(/1, 2, -1/), DIM=2)
  c = cshift(c, SHIFT=(/1, 2, -1/), DIM=5)


  c = eoshift(c, SHIFT=(/1, 2, -1/), DIM=-1)
  c = eoshift(c, SHIFT=(/1, 2, -1/), DIM=0)
  c = eoshift(c, SHIFT=(/1, 2, -1/), DIM=2)
  c = eoshift(c, SHIFT=(/1, 2, -1/), DIM=5)



  print *, findloc(array1, i, -1)
  print *, findloc(array1, i, 0)
  print *, findloc(array1, i, 1)
  print *, findloc(array1, i, 10)


  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *,  maxloc(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *,  maxloc(array1, 0)
  print *,  maxloc(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *,  maxloc(array1, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *,  minloc(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *,  minloc(array1, 0)
  print *,  minloc(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *,  minloc(array1, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *,  maxval(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *,  maxval(array1, 0)
  print *,  maxval(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *,  maxval(array1, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *,  minval(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *,  minval(array1, 0)
  print *,  minval(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *,  minval(array1, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *,  norm2(d, -1)
  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *,  norm2(d, 0)
  print *,  norm2(d, 1)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *,  norm2(d, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *,  product(c, -1)
  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *,  product(c, 0)
  print *,  product(c, 1)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *,  product(c, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *,  size(c, -1)
  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *,  size(c, 0)
  print *,  size(c, 1)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *,  size(c, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *,  sum(c, -1)
  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *,  sum(c, 0)
  print *,  sum(c, 1)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *,  sum(c, 10)


  ! reduce, need an operation


  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *, iall(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *, iall(array1, 0)
  print *, iall(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *, iall(array1, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *, iany(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *, iany(array1, 0)
  print *, iany(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *, iany(array1, 10)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *, iparity(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *, iparity(array1, 0)
  print *, iparity(array1, 1)
  !ERROR: DIM=10 dimension is out of range for rank-3 array
  print *, iparity(array1, 10)


  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *, parity(mask, -1)
  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *, parity(mask, 0)
  print *, parity(mask, 1)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, parity(mask, 10)




end program test_dim_versus_rank

subroutine test_count_dim
  integer :: a(2,3), b(2,3)
  logical :: mask(2,3)

  print *, count(mask)
  print *, count(mask, 1)
  print *, count(mask, 2)

  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *, count(mask, 0)
  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *, count(mask, -1)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, count(mask, 10)

  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, count(a==b, 10)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, count(a/=b, 10)

  print *, count(a==b, 2)
  print *, count(a/=b, 2)

  !ERROR: DIM=10 is not valid for an array of rank 1
  !ERROR: DIM=10 dimension is out of range for rank-1 array
  print *, count([.true., .false.], 10)
  !ERROR: DIM=0 is not valid for an array of rank 1
  !ERROR: DIM=0 dimension is out of range for rank-1 array
  print *, count([.true., .false.], 0)
  !ERROR: DIM=-1 is not valid for an array of rank 1
  !ERROR: DIM=-1 dimension is out of range for rank-1 array
  print *, count([.true., .false.], -1)

end subroutine test_count_dim

subroutine test_any_dim
  integer :: a(2,3), b(2,3)
  logical :: mask(2,3)

  !ERROR: DIM=-1 dimension is out of range for rank-2 array
  print *, any(mask, -1)
  !ERROR: DIM=0 dimension is out of range for rank-2 array
  print *, any(mask, 0)
  print *, any(mask, 1)
  print *, any(mask, 2)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, any(mask, 10)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, any(a==b, 10)
  !ERROR: DIM=10 dimension is out of range for rank-2 array
  print *, any(a/=b, 10)

  print *, any(a==b, 2)
  print *, any(a/=b, 2)

  !ERROR: DIM=10 is not valid for an array of rank 1
  !ERROR: DIM=10 dimension is out of range for rank-1 array
  print *, any([.true., .false.], 10)
  !ERROR: DIM=0 is not valid for an array of rank 1
  !ERROR: DIM=0 dimension is out of range for rank-1 array
  print *, any([.true., .false.], 0)
  !ERROR: DIM=-1 is not valid for an array of rank 1
  !ERROR: DIM=-1 dimension is out of range for rank-1 array
  print *, any([.true., .false.], -1)
end subroutine test_any_dim


subroutine test_lbound_dim
  integer :: array1(2,2,2)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *, lbound(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *, lbound(array1, 0)
  print *, lbound(array1, 1)
  print *, lbound(array1, 2)
  print *, lbound(array1, 3)
  !ERROR: DIM=4 dimension is out of range for rank-3 array
  print *, lbound(array1, 4)

  !ERROR: DIM=-1 dimension is out of range for rank-1 array
  print *, lbound([.true., .false.], -1)
  !ERROR: DIM=0 dimension is out of range for rank-1 array
  print *, lbound([.true., .false.], 0)
  print *, lbound([.true., .false.], 1)
  !ERROR: DIM=10 dimension is out of range for rank-1 array
  print *, lbound([.true., .false.], 10)

end subroutine test_lbound_dim

subroutine test_ubound_dim
  integer :: array1(2,2,2)

  !ERROR: DIM=-1 dimension is out of range for rank-3 array
  print *, ubound(array1, -1)
  !ERROR: DIM=0 dimension is out of range for rank-3 array
  print *, ubound(array1, 0)
  print *, ubound(array1, 1)
  print *, ubound(array1, 2)
  print *, ubound(array1, 3)
  !ERROR: DIM=4 dimension is out of range for rank-3 array
  print *, ubound(array1, 4)

  !ERROR: DIM=-1 dimension is out of range for rank-1 array
  print *, ubound([.true., .false.], -1)
  !ERROR: DIM=0 dimension is out of range for rank-1 array
  print *, ubound([.true., .false.], 0)
  print *, ubound([.true., .false.], 1)
  !ERROR: DIM=10 dimension is out of range for rank-1 array
  print *, ubound([.true., .false.], 10)

end subroutine test_ubound_dim
