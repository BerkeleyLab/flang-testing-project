! RUN: %python %S/test_errors.py %s %flang_fc1
! XFAIL: *
! Check for semantic errors in coshape() function,
! as defined in section 16.9.55 of the Fortran
! 2018 standard

program coshape_tests
  use iso_c_binding, only : c_int32_t, c_int64_t
  implicit none

  integer array(1), non_coarray(1), scalar_coarray[*], array_coarray(1)[*], non_constant, scalar_result
  real real_coarray[*]
  complex complex_coarray[*]
  character char_array(1)
  logical non_integer, logical_coarray[*]
  integer, allocatable :: codimensions(:)

  !___ standard-conforming statement with no optional arguments present ___
  codimensions = coshape(scalar_coarray)
  codimensions = coshape(array_coarray)
  !ERROR: 'coarray=' argument must have corank > 0 for intrinsic 'coshape'
  codimensions = coshape(array_coarray(1))
  codimensions = coshape(scalar_coarray[1])
  codimensions = coshape(real_coarray)
  codimensions = coshape(logical_coarray)
  codimensions = coshape(complex_coarray)
  codimensions = coshape(coarray=scalar_coarray)

  !___ standard-conforming statements with optional kind argument present ___
  codimensions = coshape(scalar_coarray, c_int32_t)
  codimensions = coshape(real_coarray, kind=c_int32_t)
  codimensions = coshape(coarray=logical_coarray, kind=c_int32_t)
  codimensions = coshape(kind=c_int32_t, coarray=complex_coarray)

  !___ non-conforming statements ___
  !ERROR: 'coarray=' argument must have corank > 0 for intrinsic 'coshape'
  codimensions = coshape(non_coarray)

  !ERROR: Actual argument for 'kind=' has bad type 'LOGICAL(4)'
  codimensions = coshape(scalar_coarray, non_integer)

  !ERROR: 'kind=' argument must be a constant scalar integer whose value is a supported kind for the intrinsic result type
  codimensions = coshape(real_coarray, non_constant)

  !ERROR: 'kind=' argument has unacceptable rank 1
  codimensions = coshape(complex_coarray, array)

  !ERROR: missing mandatory 'coarray=' argument
  codimensions = coshape()

  !ERROR: missing mandatory 'coarray=' argument
  codimensions = coshape(kind=c_int32_t)

  !ERROR: 'coarray=' argument must have corank > 0 for intrinsic 'coshape'
  codimensions = coshape(3.4)

  !ERROR: 'coarray=' argument must have corank > 0 for intrinsic 'coshape'
  codimensions = coshape(coarray=3.4)

  !ERROR: too many actual arguments for intrinsic 'coshape'
  codimensions = coshape(scalar_coarray, c_int32_t, 0)

  !ERROR: 'coarray=' argument must have corank > 0 for intrinsic 'coshape'
  codimensions = coshape(coarray=non_coarray)

  !ERROR: unknown keyword argument to intrinsic 'coshape'
  codimensions = coshape(c=real_coarray)

  !ERROR: Actual argument for 'kind=' has bad type 'LOGICAL(4)'
  codimensions = coshape(complex_coarray, kind=non_integer)

  !ERROR: unknown keyword argument to intrinsic 'coshape'
  codimensions = coshape(logical_coarray, kinds=c_int32_t)

  !ERROR: repeated keyword argument to intrinsic 'coshape'
  codimensions = coshape(coarray=scalar_coarray, coarray=real_coarray)

  !ERROR: repeated keyword argument to intrinsic 'coshape'
  codimensions = coshape(real_coarray, kind=c_int32_t, kind=c_int64_t)

  !ERROR: No intrinsic or user-defined ASSIGNMENT(=) matches scalar INTEGER(4) and rank 1 array of INTEGER(4)
  scalar_result = coshape(scalar_coarray)

  !ERROR: No intrinsic or user-defined ASSIGNMENT(=) matches operand types CHARACTER(KIND=1) and INTEGER(4)
  char_array = coshape(real_coarray)

end program coshape_tests
