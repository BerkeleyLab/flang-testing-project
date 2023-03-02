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
    - _Intrinsic derived types:_ `event_type`, `team_type`, `lock_type`
    - _Atomic kind type parameters:_ `atomic_int_kind` and `atomic_logical_kind`
    - _Values:_ `stat_failed_image`, `stat_locked`, `stat_locked_other_image`, `stat_stopped_image`, `stat_unlocked`, `stat_unlocked_failed_image`

In addition to being able to support syntax related to the above features, compilers will also need to be able to handle new execution concepts such as image control.  The image control concept affects the behaviors of some statements that were introduced in Fortran expressly for supporting parallel programming, but image control also affects the behavior of some statements that pre-existed parallism in standard Fortran:
 * **Image control statements:**
   - _Pre-existing statements_: `allocate`, `deallocate`, `stop`, `end`, a `call` referencing `move_alloc` with coarray arguments
   - _New statements:_ `sync all`, `sync images`, `sync memory`, `sync team`, `change team`, `end team`, `critical`, `end critical`, `event post`, `event wait`, `form team`, `lock`, `unlock`
One consequence of the statements being categorized as image control statements will be the need to restrict code movement by optimizing compilers.

# Proposed solution
  This design document proposes an interface to support the above features, named Fortran Parallel Runtime Interface.  Implementations of some parts of the interface exist in [Caffeine], a parallel runtime library targeting coarray Fortran compilers.  By defining a library-agnostic interface, we envision facilitating the development of alternative parallel runtime libraries that support the same interface.  One benefit of this approach is the ability to vary the communication substrate.  For example, Caffeine uses the [GASNet-EX] exascale networking middleware, whereas it might also be possible to develop wrappers that would support the proposed interface with [OpenCoarrays], which uses the Message Passing Interface ([MPI]). A central aim of this document is to use a parallel runtime interface in standard Fortran syntax, which enables us to leverage the Fortran to succinctly express various properties of the procedure interfaces, including argument attributes.  See [Rouson and Bonachea (2022)] for additional details.

# Interface overview
  This document proposes a design for the Fortran Parallel Runtime Interface. It outlines which tasks will be the responsibility of the Fortran compiler and which tasks will be the responsibility of the runtime library. For the rest of the document, we will refer to the design in terms of Flang and Caffeine.

## Fortran Parallel Runtime Interface

## Types
(TODO: add hyperlinks to the discussion of each type description)

 Provided Fortran types
   * `caf_event_type`
   * `caf_team_type`
   * `caf_lock_type`

 Caffeine specific types
   * `caf_co_handle_t` : `caf_co_handle_t` will be a derived type provided by the runtime library and that will be opaque to the compiler.
   * `caf_async_handle_t`
   * `caf_source_loc`   (REMOVE_NOTE: something like this is needed for critical constructs) (Does compiler control implementation of the type, or just provide the information and Caffeine controls the implementation?) OR deal with critical constructs by rewriting critical constructs as blocks with lock and unlocks (BURDENSOME because lock_type has to be coarray, this is the rationale for not rewriting, if we need it)

## Common arguments

   [`coarray_handle`](#coarray_handle), [`coindices`](#coindices), [`target`](#target), [`value`](#value), [`team`](#team), [`team_number`](#team_number), [`stat`](#stat)

## Procedures

   Collectives
     [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum)

   Program startup and shutdown
     [`caf_init`](#caf_init), [`caf_finalize`](#caf_finalize), [`caf_error_stop`](#caf_error_stop), [`caf_stop`](#caf_stop), [`caf_fail_image`](#caf_fail_image)

   Allocation and deallocation
     [`caf_allocate`](#caf_allocate), [`caf_deallocate`](#caf_deallocate)

   Coarray Access
     [`caf_put`](#caf_put), [`caf_get_blocking`](#caf_get_blocking), [`caf_get_async`](#caf_get_async)

   Operation Synchronization
     [`caf_async_wait_for`](#caf_aync_wait_for), [`caf_async_try_for`](#caf_async_try_for), [`caf_sync_memory`](#caf_sync_memory)

   Image Synchronization
     [`caf_sync_all`](#caf_sync_all), [`caf_sync_images`](#caf_sync_images), [`caf_lock`](#caf_lock), [`caf_unlock`](#caf_unlock), [`caf_critical`](#caf_critical)

   Events
     [`caf_event_post`](#caf_event_post), [`caf_event_wait`](#caf_event_wait), [`caf_event_query`](#caf_event_query)

   Teams
     [`caf_change_team`](#caf_change_team), [`caf_end_team`](#caf_end_team), [`caf_form_team`](#caf_form_team), [`caf_sync_team`](#caf_sync_team), [`caf_get_team`](#caf_get_team), [`caf_team_number`](#caf_team_number)

   Atomic Memory Operation
     [`caf_atomic_add`](#caf_atomic_add), [`caf_atomic_and`](#caf_atomic_and), [`caf_atomic_cas`](#caf_atomic_cas), [`caf_atomic_define`](#caf_atomic_define), [`caf_atomic_fetch_add`](#caf_atomic_fetch_add), [`caf_atomic_fetch_and`](#caf_atomic_fetch_and), [`caf_atomic_fetch_or`](#caf_atomic_fetch_or), [`caf_atomic_fetch_xor`](#caf_atomic_fetch_xor), [`caf_atomic_or`](#caf_atomic_or), [`caf_atomic_ref`](#caf_atomic_ref), [`caf_atomic_xor`](#caf_atomic_xor)

   Coarray Queries
     [`caf_lcobound`](#caf_lcobound), [`caf_ucobound`](#caf_ucobound), [`caf_coshape`](#caf_coshape), [`caf_image_index`](#caf_image_index)

   Image Queries
     [`caf_num_images`](#caf_num_images), [`caf_this_image`](#caf_this_image), [`caf_failed_images`](#caf_failed_images), [`caf_stopped_images`](#caf_stopped_images), [`caf_image_status`](#caf_image_status)


## Common arguments' descriptions

 ### `coarray_handle`
   * Argument for [`caf_put`](#caf_put), [`caf_get_blocking`](#caf_get_blocking)
   * scalar of type `caf_co_handle_t`
   * This argument is a handle for the established coarray. The handle will be created when the coarray is established.
 ### `coarray_handles`
   * array of type `caf_co_handle_t`
 ### `coindices`
   * 1d array of type `integer`, dimension(:)
 ### `target`
   * 1d array of type(*), dimension(..)
 ### `value`
 ### `team`
 ### `team_number`
 ### `stat`
  * Argument for [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum)
  * of type `integer`
  * if no error condition occurs on that image, it is assigned the value `0`

## Procedure descriptions

### Collectives

 #### `caf_co_broadcast`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**: [`stat`](#stat)

 #### `caf_co_max`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**: [`stat`](#stat)

 #### `caf_co_min`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**: [`stat`](#stat)

 #### `caf_co_reduce`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**: [`stat`](#stat)

 #### `caf_co_sum`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**: [`stat`](#stat)

### Program startup and shutdown

  When the compiler identifies a program that uses "Coarray Fortran" features, it will insert calls to `caf_init` and `caf_finalize`. These procedures ...

 #### `caf_init`
  * **Description**: (REMOVE_NOTE: should it be caf_caffeinate?)
  * **Procedure Interface**: `function caf_init() result(exit_code)`
  * **Result**: `exit_code` is an `integer` whose value ...

 #### `caf_finalize`
  * **Description**: (REMOVE_NOTE: should it be caf_decaffeinate?)
  * **Procedure Interface**: `subroutine caf_finalize(exit_code)`
  * **Arguments**: `exit_code` is `intent(in)` and ...

 #### `caf_error_stop`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_stop`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_fail_image`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Allocation and deallocation

 #### `caf_allocate`
  * **Description**: Calls to `caf_allocate` will be inserted when the compiler wants to allocate a coarray or when there is a statically declared coarray. This procedure allocates memory for a coarray.
  * **Procedure Interface**: 2 options, see link to pseudo code, based on (yet undecided) choice between whether the compiler implements `image_index`, `coshape`, `lcobound`, and `ucobound` or whether the runtime library implements those
  * **Arguments**:
  * [caf_allocate pseudo code](#caf_allocate-pseudo-code) (temporarily in design doc)

 #### `caf_deallocate`
  * **Description**:
  * **Procedure Interface**: `subroutine caf_deallocate(coarray_handles)`
  * **Arguments**: `coarray_handles` is `intent(out)`

### Coarray Access

 Coarray accesses will maintain serial dependencies for the issuing image. A non-blocking get has to be started and finished in the same segment. The interface provides puts that are fence-based and gets that are split phased.


 #### `caf_put`
  * **Description**:
  * **Procedure Interface**: `subroutine caf_put(coarray_handle, coindices, team, team_number, target, value, stat)`
  * **Arguments**: [`coarray_handle`](#coarray_handle) is `intent(in)`, [`coindices`](#coindices) is `intent(in)`, [`target`](#target) is `intent(in)`
  * [caf_put pseudo code](#caf_put-pseudo-code) (temporarily in design doc)

 #### `caf_get_blocking`
  * **Description**:
  * **Procedure Interface**: `subroutine caf_get_blocking(coarray_handle, coindices, team, team_number, source, value, stat)`
  * **Arguments**: [`coarray_handle`](#coarray_handle) is `intent(in)`, [`coindices`](#coindices) is `intent(in)`, [`target`](#target) is `intent(in)`

 #### `caf_get_async`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

###  Operation Synchronization

 #### `caf_async_wait_for`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:
  * [caf_async_wait_for pseudo code](#caf_async_wait_for-pseudo-code) (temporarily in design doc)

 #### `caf_async_try_for`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:
  * [caf_async_try_for pseudo code](#caf_async_try_for-pseudo-code) (temporarily in design doc)

 #### `caf_sync_memory`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Image Synchronization

 #### `caf_sync_all`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_sync_images`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_lock`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_unlock`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_critical`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Events

 #### `caf_event_post`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_event_wait`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_event_query`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Teams

 #### `caf_change_team`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_end_team`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_form_team`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_sync_team`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_get_team`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_team_number`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Atomic Memory Operation

 #### `caf_atomic_add`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_and`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_cas`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_define`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_fetch_add`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_fetch_and`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_fetch_or`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_fetch_xor`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_or`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_ref`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_atomic_xor`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Coarray Queries

 #### `caf_lcobound`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_ucobound`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_coshape`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_image_index`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

### Image Queries

 #### `caf_num_images`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_this_image`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_failed_images`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_stopped_images`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:

 #### `caf_image_status`
  * **Description**:
  * **Procedure Interface**:
  * **Arguments**:


### Allocation and deallocation


```
  module subroutine caf_deallocate(coarray_handles)
    implicit none
    type(caf_co_handle_t), dimension(:), intent(out) :: coarray_handles
  end subroutine
```

### Puts and Gets

  Arguments to `caf_put_blocking` and `caf_get_blocking`:

REMOVE_NOTE: remove following table as integrate information into above argument descriptions

| Argument | Type | Rank | Dimensions | Intent | Additional attributes | Notes |
| -------- | ---- | ---- | ---------- | ------ | --------------------- | ----- |
| `target` | `type(*)` | 1 | dimension(..) | `intent(in)` | n/a | ----- |
| `value`  | `type(*)` | 1 | dimension(..) | `intent(in)` for gets, `intent(inout)` for puts | n/a | ----- |
| `team` | `team_type` | 0 | n/a | `intent(in)` | optional | Both optional arguments `team` and `team_number` shall not be present in the same call|
| `team_number` |  `integer` | 0 | n/a | `intent(in)` | optional | Both optional arguments `team` and `team_number` shall not be present in the same call|
| `stat` | `integer` | 0 | n/a | `intent(out)` | optional | ----- |

  * **Asynchrony:**
    -   Could be handle based or fence based approaches
    -   Handle based - return can individual operation handle, later on compiler synchronizes handle
    -   Fence based - implicit handle operations, closer to MPI
#
## Atomic subroutines

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
    -   Procedure Interface:   `subroutine caf_atomic_add(coarray_handle, coindicies, offset, value, stat)` or `subroutine caf_atomic_add(coarray_handle, coindicies, target, value, stat)`
    -   Arguments: ...


Current pseudo code. May not stay in design doc.

Option 1 with offset:
```
  module subroutine caf_atomic_add(coarray_handle, coindicies, offset, value, stat) ! blocking atomic operation
    type(caf_co_handle_t) :: coarray_handle
    integer, intent(in) :: coindices(:)
    integer :: offset, value, stat
  end subroutine
```

Option 2 with target:
```
  module subroutine caf_atomic_add(coarray_handle, coindicies, target, value, stat) ! blocking atomic operation
    type(caf_co_handle_t) :: coarray_handle
    integer, intent(in) :: coindices(:) ! names image num
    integer(kind=atomic_int_kind), intent(in) :: target !location of target is relevant, not the value of target, need this to compute the offset when the `atom` dummy argument to the intrinsic is part of a derived type
    integer :: value, stat
  end subroutine
```


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
| Reference a coindexed-object             |           |     ✓     |


Add to table: teams, events, synchronization statements, critical construct, locks





Current pseudo code. May not stay in design doc.

Draft:

#### caf_allocate pseudo code

Compiler-tracks-codescriptor
```
  module subroutine caf_allocate(coarray_handle, local_slice)
    implicit none
    type(caf_co_handle_t), intent(out) :: coarray_handle
    type(*), dimension(..), intent(inout) :: local_slice
  end subroutine
```
In this case, compiler would provide `image_index`, `coshape`, `lcobound`, `ucobound`

Caffeine-tracks-codescriptor
```
  module subroutine caf_allocate(lbounds, sizes, coarray_handle, local_slice)
    implicit none
    type(caf_co_handle_t), intent(out) :: coarray_handle
    type(*), dimension(..), intent(inout) :: local_slice
    integer, dimension(:), intent(in) :: lbounds, sizes !precondition these args must be same size
  end subroutine
```
In this case, Caffeine would provide `image_index`, `coshape`, `lcobound`, `ucobound`


#### caf_put pseudo code

```
  module subroutine caf_put(coarray_handle, coindices, team, team_number, target, value, stat)
    implicit none
    type(caf_co_handle_t), intent(in) :: coarray_handle
    integer, intent(in) :: coindices(:)
    type(*), dimension(..), intent(in) :: target, value
    type(team_type), optional, intent(in) :: team
    integer, optional, intent(in) :: team_number
    integer, optional, intent(out) :: stat
  end subroutine
```

#### caf_end_segment pseudo code

```
  ! any puts that are still in flight need to commited
  ! throw away any caches
  ! not synchronizing operation
  ! caf_end_segment is a side effect of image control stmts
  module subroutine caf_end_segment()
    implicit none
  end subroutine
```

#### caf_get_blocking pseudo code

```
  module subroutine caf_get_blocking(coarray_handle, coindices, team, team_number, source, value, stat)
    implicit none
    type(caf_co_handle_t), intent(in) :: coarray_handle
    integer, intent(in) :: coindices(:)
    type(*), dimension(..), intent(in) :: source ! useful to get the "shape" of the thing, not the value of this dummy arg, compiler needs to ensure this dummy arg is not a copy for this strategy to work, compiler's codegen needs to ensure that this (and other subroutine calls) are not using copies for this arg
    type(*), dimension(..), intent(inout) :: value
    type(team_type), optional, intent(in) :: team
    integer, optional, intent(in) :: team_number
    integer, optional, intent(out) :: stat
  end subroutine
```

#### caf_get_async pseudo code

```
  module subroutine caf_get_async(coarray_handle, coindices, team, team_number, source, value, stat, async_handle)
    implicit none
    type(caf_co_handle_t), intent(in) :: coarray_handle
    integer, intent(in) :: coindices(:)
    type(*), dimension(..), intent(in) :: source
    type(*), dimension(..), intent(inout) :: value ! may need asynchronous attribute or may be implicitly asynchronous
    type(team_type), optional, intent(in) :: team
    integer, optional, intent(in) :: team_number
    integer, optional, intent(out) :: stat
    type(caf_async_handle_t), intent(out) :: async_handle
  end subroutine
```

#### caf_async_wait_for pseudo code

```
  ! waits until operation
  ! consumes handle
  module subroutine caf_wait_for(async_handle)
    implicit none
    type(caf_async_handle_t), intent(inout) :: async_handle
  end subroutine
```

#### caf_async_try_for pseudo code

```
  ! consumes handle IF finished
  module subroutine caf_try_for(async_handle, finished)
    implicit none
    type(caf_async_handle_t), intent(inout) :: async_handle
    logical, intent(out) :: finished
  end subroutine

```






## Berkeley Lab internal Notes: (REMOVE_NOTES before submission)

Same non-blocking semantics (has to be started and finished in the same segment) will likely apply to collectives, use caf_wait_for, caf_try_for, etc
Should change team and critical be non-blocking? sync-all?)



### `caf_co_handle_t`

   The following is a Fortran heavy pseudo code, not the exact implementation we plan
   ```
   type caf_co_handle_t
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
    if the constants (stat_failed_image, etc) are compiler provided, we need to get C access to these values

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
