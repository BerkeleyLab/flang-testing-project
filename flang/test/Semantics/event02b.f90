! RUN: %python %S/test_errors.py %s %flang_fc1
! This test checks for semantic errors in event wait statements based on the
! statement specification in section 11.6.8 of the Fortran 2018 standard.
! Some of the errors in this test would be hidden by the errors in
! the test event02a.f90 if they were included in that file,
! and are thus tested here.

program test_event_wait
  use iso_fortran_env, only : event_type
  implicit none

  ! event_type variables must be coarrays
  type(event_type) non_coarray

  type(event_type) concert[*], occurrences(2)[*]
  integer threshold, indexed(1), non_event[*], sync_status, co_indexed_integer[*], superfluous_stat, non_scalar(1)
  character(len=128) error_message, non_scalar_char(1), co_indexed_character[*], superfluous_errmsg
  logical invalid_type

  !____________________ non-standard-conforming statements __________________________

  !_________________________ invalid event-variable ________________________________

  !ERROR: The event-variable must be of type EVENT_TYPE from module ISO_FORTRAN_ENV
  event wait(non_event)

  ! event-variable must be a coarray
  event wait(non_coarray)

  !ERROR: A event-variable in a EVENT WAIT statement may not be a coindexed object
  event wait(concert[1])

  !ERROR: A event-variable in a EVENT WAIT statement may not be a coindexed object
  event wait(occurrences(1)[1])

  !ERROR: Must be a scalar value, but is a rank-1 array
  event wait(occurrences)

  !_____________ invalid event-wait-spec-lists: invalid until-spec _________________

  !ERROR: Must have INTEGER type, but is LOGICAL(4)
  event wait(concert, until_count=invalid_type)

  !ERROR: Must be a scalar value, but is a rank-1 array
  event wait(concert, until_count=non_scalar)

  !_________________ invalid event-wait-spec-lists: invalid stat= ________________________

  !ERROR: Must have INTEGER type, but is LOGICAL(4)
  event wait(concert, stat=invalid_type)

  !ERROR: Must be a scalar value, but is a rank-1 array
  event wait(concert, stat=non_scalar)

  !________________ invalid event-wait-spec-lists: invalid errmsg= _______________________

  !ERROR: Must have CHARACTER type, but is LOGICAL(4)
  event wait(concert, errmsg=invalid_type)

  !ERROR: Must be a scalar value, but is a rank-1 array
  event wait(concert, errmsg=non_scalar_char)

  !______ invalid event-wait-spec-lists: redundant event-wait-spec-list ____________

  !ERROR: Until-spec in a event-wait-spec-list may not be repeated
  event wait(concert, until_count=threshold, until_count=indexed(1))

  !ERROR: A stat-variable in a event-wait-spec-list may not be repeated
  event wait(concert, stat=sync_status, stat=superfluous_stat)

  !ERROR: A errmsg-variable in a event-wait-spec-list may not be repeated
  event wait(concert, errmsg=error_message, errmsg=superfluous_errmsg)

  !ERROR: Until-spec in a event-wait-spec-list may not be repeated
  event wait(concert, stat=sync_status, errmsg=error_message, until_count=threshold, until_count=indexed(1))

  !ERROR: A stat-variable in a event-wait-spec-list may not be repeated
  event wait(concert, stat=sync_status, errmsg=error_message, until_count=threshold, stat=superfluous_stat)

  !ERROR: A errmsg-variable in a event-wait-spec-list may not be repeated
  event wait(concert, stat=sync_status, errmsg=error_message, until_count=threshold, errmsg=superfluous_errmsg)

  !ERROR: A stat-variable in a event-wait-spec-list may not be repeated
  !ERROR: A errmsg-variable in a event-wait-spec-list may not be repeated
  event wait(concert, stat=sync_status, errmsg=error_message, until_count=threshold, stat=superfluous_stat, errmsg=superfluous_errmsg)

  !ERROR: A errmsg-variable in a event-wait-spec-list may not be repeated
  !ERROR: A stat-variable in a event-wait-spec-list may not be repeated
  !ERROR: Until-spec in a event-wait-spec-list may not be repeated
  event wait(concert, stat=sync_status, errmsg=error_message, until_count=threshold, errmsg=superfluous_errmsg, stat=superfluous_stat, until_count=indexed(1))

  !_____________ invalid event-wait-spec-lists: coindexed objects __________________

  !ERROR: A stat-variable or errmsg-variable in a event-wait-spec-list may not be a coindexed object
  event wait(concert, stat=co_indexed_integer[1])

  !ERROR: A stat-variable or errmsg-variable in a event-wait-spec-list may not be a coindexed object
  event wait(concert, errmsg=co_indexed_character[1])

end program test_event_wait
