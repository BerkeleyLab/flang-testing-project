<!--===- docs/CoarrayFortranRuntime.md

   Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
   See https://llvm.org/LICENSE.txt for license information.
   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

-->

# Problem description
  In order to be fully Fortran 2018 compliant, Flang needs to add support for what is commonly referred to as coarray fortran, which includes features related to parallelism. These features include the following statements, subroutines, functions, types, and kind type parameters:

  * **Statements:**
    - _Synchronization:_ `sync all`, `sync images`, `sync memory`, `sync team`
    - _Events:_ `event post`, `event wait`
    - _Error termination:_ `error stop`
    - _Locks:_ `lock`, `unlock`
    - _Failed images:_ `fail image`
    - _Teams:_ `form team`, `change team`
    - _Critical sections:_ `critical`, `end critical`
  * **Intrinsic functions:**
  `num_images`, `this_image`, `lcobound`, `ucobound`, `team_number`, `get_team`, `failed_images`, `stopped_images`,
  `image_status`, `coshape`, `image_index`
  * **Intrinsic subroutines:**
    - _Collective subroutines:_ `co_sum`, `co_max`, `co_min`, `co_reduce`, `co_broadcast`
    - _Atomic subroutines:_ `atomic_add`, `atomic_and`, `atomic_cas`, `atomic_define`,
  `atomic_fetch_add`, `atomic_fetch_and`, `atomic_fetch_or`, `atomic_fetch_xor`, `atomic_or`, `atomic_ref`, `atomic_xor`
    - _Other subroutines:_ `event_query`
  * **Types, kind type parameters, and values:**
    - _Intrinsic derived types:_ `event_type`, `team_type`
    - _Atomic kind type parameters:_ `atomic_int_kind` and `atomic_logical_kind`
    - _Values:_ `stat_failed_image`, `stat_locked`, `stat_locked_other_image`, `stat_stopped_image`, `stat_unlocked`, `stat_unlocked_failed_image`

In addition to being able to support syntax related to the above features, compilers will also need to be able to handle new execution concepts such as image control.  The image control concept affects the behaviors of some statements that were introduced in Fortran expressly for supporting parallel programming, but image control also affects the behavior of some statements that pre-existed parallism in standard Fortran:
 * **Image control statements:**
   - _Pre-existing statements_: `allocate`, `deallocate`, `stop`, `end`, a `call` referencing `move_alloc` with coarray arguments
   - _New statements:_ `sync all`, `sync images`, `sync memory`, `sync team`, `change team`, `end team`, `critical`, `end critical`, `event post`, `event wait`, `form team`, `lock`, `unlock`
One consequence of the statements being categorizing statements as image control will be the need to restrict code movement by optimizing compilers.

# Proposed solution
  This design document proposes an application programming interface (API) to support the above features.  Implementations of some parts of the API exist in [Caffeine], a parallel runtime library targeting coarray Fortran compilers.  By defining a library-agnostic API, we envision facilitating the development of alternative parallel runtime libraries that support the same API.  One benefit of this approach is the ability to vary the communication substrate.  For example, Caffeine uses the [GASNet-EX] exascale networking middleware, whereas it might also be possible to develop wrappers that would support the proposed API with [OpenCoarrays], which uses the Message Passing Interface ([MPI]). A central aim of this document is to use a parallel runtime API in standard Fortran syntax, which enables us to leverage the Fortran to succinctly express various properties of the procedure interfaces, including argument attributes.  See [Rouson and Bonachea (2022)] for additional details.

# Implementation details overview
  This design document proposes the design of Flang features and discusses how Flang will interface with Caffeine, the
  runtime library. It outlines which tasks will be the responsibility of Flang and which tasks will be the responsibility
  of Caffeine.

## Coarray Runtime Library Caffeine
  Caffeine is a parallel runtime library that aims to support Fortran compilers with a programming-model-agnostic application
  binary interface (ABI) to various communication libraries. Current work is on supporting the ABI with the GASNet-EX
  exascale-ready networking middleware.

## Delegation of tasks between Flang and Caffeine

| Tasks | Flang | Caffeine |
| ----  | ----- | -------- |
| Track corank of coarrays                |     ✓     |           |
| Track teams associated with a coarray   |     ✓     |           |
| Assigning variables of type `team-type` |     ✓     |           |
| Translate critical construct to lock/unlock |     ✓     |           |
| Track coarrays for implicit deallocation when exiting a scope |     ✓     |           |
| Initialize a coarray with SOURCE= as part of allocate-stmt |     ✓     |           |
| Keeping track of corank |     ✓     |     ?      |
| Implementing the intrinsics `coshape`, `lcobound`, and `ucobound`, `image_index`  |     ?     |     ?     |
| Track allocatable coarrays for implicit deallocation at `end-team-stmt`  |           |     ✓     |
| Team stack abstraction                  |           |     ✓     |
| `form-team-stmt`                        |           |     ✓     |
| `change-team-stmt`                      |           |     ✓     |
| `end-team-stmt`                         |           |     ✓     |
| Allocate a coarray                      |           |     ✓     |
| Deallocate a coarray                    |           |     ✓     |
| Reference a coindexed-object           |           |     ✓     |


Add to table: teams, events, synchronization statements, critical construct, locks

## Compiler facing Caffeine API


### Allocation and deallocation

Draft:

caf_allocate is called when the compiler wants to allocate a coarray, or when there is a statically declared coarray

Compiler-tracks-codescriptor
```
  module subroutine caf_allocate(coarray_handle, local_slice)
    implicit none
    type(caf_co_handle), intent(out) :: coarray_handle
    type(*), dimension(..), intent(inout) :: local_slice
  end subroutine
```
In this case, compiler would provide `image_index`, `coshape`, `lcobound`, `ucobound`

Caffeine-tracks-codescriptor
```
  module subroutine caf_allocate(lbounds, sizes, coarray_handle, local_slice)
    implicit none
    type(caf_co_handle), intent(out) :: coarray_handle
    type(*), dimension(..), intent(inout) :: local_slice
    integer, dimension(:), intent(in) :: lbounds, sizes !precondition these args must be same size
  end subroutine
```
In this case, Caffeine would provide `image_index`, `coshape`, `lcobound`, `ucobound`


```
  module subroutine caf_deallocate(coarray_handles)
    implicit none
    type(caf_co_handle), dimension(:), intent(out) :: coarray_handles
  end subroutine
```

### Puts and Gets


Semantics: Puts and gets will maintain serial dependencies for the issuing image.
           A non-blocking get has to be started and finished in the same segment.
           (same non-blocking semantics will likely apply to collectives, use caf_wait_for, caf_try_for, etc)
           (should change team and critical be non-blocking? sync-all?)

Current pseudo code. May not stay in design doc.

Will have fence based puts
split phased gets

```
  module subroutine caf_put(coarray, coindices, team, team_number, target, value, stat)
    implicit none
    type(caf_co_handle), intent(in) :: coarray
    integer, intent(in) :: coindices(:)
    type(*), dimension(..), intent(in) :: target, value
    type(team_type), optional, intent(in) :: team
    integer, optional, intent(in) :: team_number
    integer, optional, intent(out) :: stat
  end subroutine

  ! any puts that are still in flight need to commited
  ! throw away any caches
  ! not synchronizing operation
  ! caf_end_segment is a side effect of image control stmts
  module subroutine caf_end_segment()
    implicit none
  end subroutine

  module subroutine caf_get_blocking(coarray, coindices, team, team_number, source, value, stat)
    implicit none
    type(caf_co_handle), intent(in) :: coarray
    integer, intent(in) :: coindices(:)
    type(*), dimension(..), intent(in) :: source ! useful to get the "shape" of the thing, not the value of this dummy arg, compiler needs to ensure this dummy arg is not a copy for this strategy to work, compiler's codegen needs to ensure that this (and other subroutine calls) are not using copies for this arg
    type(*), dimension(..), intent(inout) :: value
    type(team_type), optional, intent(in) :: team
    integer, optional, intent(in) :: team_number
    integer, optional, intent(out) :: stat
  end subroutine

  module subroutine caf_get_non_blocking(coarray, coindices, team, team_number, source, value, stat, async_handle)
    implicit none
    type(caf_co_handle), intent(in) :: coarray
    integer, intent(in) :: coindices(:)
    type(*), dimension(..), intent(in) :: source
    type(*), dimension(..), intent(inout) :: value ! may need asynchronous attribute or may be implicitly asynchronous
    type(team_type), optional, intent(in) :: team
    integer, optional, intent(in) :: team_number
    integer, optional, intent(out) :: stat
    type(caf_async_handle), intent(out) :: async_handle
  end subroutine

  ! waits until operation
  ! consumes handle
  module subroutine caf_wait_for(async_handle)
    implicit none
    type(caf_async_handle), intent(inout) :: async_handle
  end subroutine

  ! consumes handle IF finished
  module subroutine caf_try_for(async_handle, finished)
    implicit none
    type(caf_async_handle), intent(inout) :: async_handle
    logical, intent(out) :: finished
  end subroutine

```
  * **caf_get_blocking:**
    -   Description: ...
    -   Procedure Interface: `subroutine caf_get_blocking(coarray, coindices, team, team_number, source, value, stat)`

  Arguments to `caf_put_blocking` and `caf_get_blocking`:

| Argument | Type | Rank | Dimensions | Intent | Additional attributes | Notes |
| -------- | ---- | ---- | ---------- | ------ | --------------------- | ----- |
| `coarray` | `caf_co_handle` | 0 | n/a | `intent(in)` | n/a | caf_co_handle will be a derived type provided by Caffeine. This argument is a handle for the established coarray. This handle will be created when the coarray is established. |
| `coindices` | `integer` | 1 | dimension(:) | `intent(in)` | n/a | ----- |
| `target` | `type(*)` | 1 | dimension(..) | `intent(in)` | n/a | ----- |
| `value`  | `type(*)` | 1 | dimension(..) | `intent(in)` for gets, `intent(inout)` for puts | n/a | ----- |
| `team` | `team_type` | 0 | n/a | `intent(in)` | optional | Both optional arguments `team` and `team_number` shall not be present in the same call|
| `team_number` |  `integer` | 0 | n/a | `intent(in)` | optional | Both optional arguments `team` and `team_number` shall not be present in the same call|
| `stat` | `integer` | 0 | n/a | `intent(out)` | optional | ----- |

  * **Asynchrony:**
    -   Could be handle based or fence based approaches
    -   Handle based - return can individual operation handle, later on compiler synchronizes handle
    -   Fence based - implicit handle operations, closer to MPI

### Atomic subroutines

  * **caf_atomic_define:**
    -   Description: ...
    -   Procedure Interface: ...
    -   Arguments: ...

  * **caf_atomic_ref:**
    -   Description: ...
    -   Procedure Interface: ...
    -   Arguments: ...

  * **caf_atomic_add:**
    -   Description: Blocking atomic operation...
    -   Procedure Interface:   `subroutine caf_atomic_add(coarray, coindicies, offset, value, stat)` or `subroutine caf_atomic_add(coarray, coindicies, target, value, stat)`
    -   Arguments: ...


Current pseudo code. May not stay in design doc.

Option 1 with offset:
```
  module subroutine caf_atomic_add(coarray, coindicies, offset, value, stat) ! blocking atomic operation
    type(caf_co_handle) :: coarray
    integer, intent(in) :: coindices(:)
    integer :: offset, value, stat
  end subroutine
```

Option 2 with target:
```
  module subroutine caf_atomic_add(coarray, coindicies, target, value, stat) ! blocking atomic operation
    type(caf_co_handle) :: coarray
    integer, intent(in) :: coindices(:) ! names image num
    integer(kind=atomic_int_kind), intent(in) :: target !location of target is relevant, not the value of target, need this to compute the offset when the `atom` dummy argument to the intrinsic is part of a derived type
    integer :: value, stat
  end subroutine
```



## Berkeley Lab internal Notes: (REMOVE before submission)

### `caf_co_handle`

   The following is a Fortran heavy pseudo code, not the exact implementation we plan
   ```
   type caf_co_handle
     type(c_ptr) :: base_addr
     integer, allocatable, dimension(:) :: lbounds, sizes
     !integer :: established_team ! probably not necessary unless we want to bounds check
   end type
   ```

TODOs:
    allow for non-blocking collective subroutines
    need to be able to track puts in flight, may need a write buffer, record boundaries in a hash table struct
    every single rma needs to check the table to see if there is a conflicting overlap
    could add caching

flexible array member in c

### Caffeine internals for coarray accesses
  Coarray access could start with image_index, with the same coarray coindicies and team identifier argument
  and would get back a single integer, which is the image number in that team
  Then can use that to pass into internal query about team and global image index and can use it to bounds check.


# Testing plan
[tbd]

[Caffeine]: https://go.lbl.gov/caffeine
[GASNet-EX]: https://go.lbl.gov/gasnet
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[MPI]: https://www.mpi-forum.org
[Rouson and Bonachea (2022)]: https://doi.org/10.25344/S4459B
