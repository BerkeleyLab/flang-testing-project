<!--===- docs/CoarrayFortranRuntime.md

   Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
   See https://llvm.org/LICENSE.txt for license information.
   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

-->
# THIS IS A WORK IN PROGRESS - DECISIONS REGARDING THE DESIGNS DISCUSSED IN THIS DOCUMENT ARE ONGOING AND MAY CHANGE AND THE DOCUMENT IS INCOMPLETE


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
  This design document proposes an interface to support the above features, named Coarray Fortran Parallel Runtime Interface. By defining a library-agnostic interface, we envision facilitating the development of alternative parallel runtime libraries that support the same interface.  One benefit of this approach is the ability to vary the communication substrate. A central aim of this document is to use a parallel runtime interface in standard Fortran syntax, which enables us to leverage Fortran to succinctly express various properties of the procedure interfaces, including argument attributes.  See [Rouson and Bonachea (2022)] for additional details.

## Coarray Fortran (CAF) Parallel Runtime Interface

  The Coarray Fortran Parallel Runtime Interface is a proposed interface in which the runtime library is responsible for coarray allocation, deallocation and accesses, image synchronization, atomic operations, events, and teams. In this interface, the compiler is responsible for transforming the source code to add Fortran procedure calls to the necessary runtime library procedures. Below you can find a table showing the delegation of tasks between the compiler and the runtime library. The interface is designed for portability across shared and distributed memory machines, different operating systems, and multiple architectures. The Caffeine implementation,[see below](#caffeine-lbl's-implementation-of-the-coarray-fortran-parallel-runtime-interface), of the Coarray Fortran Parallel Runtime Interface plans to support the following architectures: x86_64, PowerPC64, AArch64, with the possibility of supporting more as requested. Implementations of this interface is intended as an augmentation for the compiler's own runtime library. While the interface can support multiple implementations, we envision needing to build the runtime library as part of installing the compiler.

## Delegation of tasks between the Fortran compiler and the runtime library

The following table outlines which tasks will be the responsibility of the Fortran compiler and which tasks will be the responsibility of the runtime library.

| Tasks | Fortran compiler | Runtime library |
| ----  | ----- | -------- |
| Establish and initialize static coarrays prior to `main` -[see more](#establish-and-initialize-static-coarrays-priorto-`main`)        |     ✓     |           |
| Track corank of coarrays                |     ✓     |           |
| Assigning variables of type `team-type` |     ✓     |           |
| Track locals coarrays for implicit deallocation when exiting a scope |     ✓     |           |
| Initialize a coarray with SOURCE= as part of allocate-stmt |     ✓     |           |
| Implementing the intrinsics `coshape`, `lcobound`, and `ucobound`, `image_index`  |          |     ✓     |
| Track coarrays for implicit deallocation at `end-team-stmt`  |           |     ✓     |
| Team stack abstraction                  |           |     ✓     |
| `form-team-stmt`                        |           |     ✓     |
| `change-team-stmt`                      |           |     ✓     |
| `end-team-stmt`                         |           |     ✓     |
| Allocate a coarray                      |           |     ✓     |
| Deallocate a coarray                    |           |     ✓     |
| Reference a coindexed-object            |           |     ✓     |

## Types

 **Provided Fortran types:** [`caf_event_type`](#caf_event_type), [`caf_team_type`](#caf_team_type), [`caf_lock_type`](#caf_lock_type)

 **Runtime library specific types:** [`caf_co_handle_t`](#caf_co_handle_t), [`caf_async_handle_t`](#caf_async_handle_t), [`caf_source_loc_t`](#caf_source_loc_t)

## Common arguments

   [`coarray_handle`](#coarray_handle), [`coindices`](#coindices), [`target`](#target), [`value`](#value), [`team`](#team), [`team_number`](#team_number), [`stat`](#stat)

## Procedures

   **Collectives:**
     [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum)

   **Program startup and shutdown:**
     [`caf_init`](#caf_init), [`caf_finalize`](#caf_finalize), [`caf_error_stop`](#caf_error_stop), [`caf_stop`](#caf_stop), [`caf_fail_image`](#caf_fail_image)

   **Allocation and deallocation:**
     [`caf_allocate`](#caf_allocate), [`caf_deallocate`](#caf_deallocate)

   **Coarray Access:**
     [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)

   **Operation Synchronization:**
     [`caf_async_wait_for`](#caf_aync_wait_for), [`caf_async_try_for`](#caf_async_try_for), [`caf_sync_memory`](#caf_sync_memory)

   **Image Synchronization:**
     [`caf_sync_all`](#caf_sync_all), [`caf_sync_images`](#caf_sync_images), [`caf_lock`](#caf_lock), [`caf_unlock`](#caf_unlock), [`caf_critical`](#caf_critical)

   **Events:**
     [`caf_event_post`](#caf_event_post), [`caf_event_wait`](#caf_event_wait), [`caf_event_query`](#caf_event_query)

   **Teams:**
     [`caf_change_team`](#caf_change_team), [`caf_end_team`](#caf_end_team), [`caf_form_team`](#caf_form_team), [`caf_sync_team`](#caf_sync_team), [`caf_get_team`](#caf_get_team), [`caf_team_number`](#caf_team_number)

   **Atomic Memory Operation:**
     [`caf_atomic_add`](#caf_atomic_add), [`caf_atomic_and`](#caf_atomic_and), [`caf_atomic_cas`](#caf_atomic_cas), [`caf_atomic_define`](#caf_atomic_define), [`caf_atomic_fetch_add`](#caf_atomic_fetch_add), [`caf_atomic_fetch_and`](#caf_atomic_fetch_and), [`caf_atomic_fetch_or`](#caf_atomic_fetch_or), [`caf_atomic_fetch_xor`](#caf_atomic_fetch_xor), [`caf_atomic_or`](#caf_atomic_or), [`caf_atomic_ref`](#caf_atomic_ref), [`caf_atomic_xor`](#caf_atomic_xor)

   **Coarray Queries:**
     [`caf_lcobound`](#caf_lcobound), [`caf_ucobound`](#caf_ucobound), [`caf_coshape`](#caf_coshape), [`caf_image_index`](#caf_image_index)

   **Image Queries:**
     [`caf_num_images`](#caf_num_images), [`caf_this_image`](#caf_this_image), [`caf_failed_images`](#caf_failed_images), [`caf_stopped_images`](#caf_stopped_images), [`caf_image_status`](#caf_image_status)


### Caffeine - LBL's Implementation of the Coarray Fortran Parallel Runtime Interface
  Implementations of some parts of the Coarray Fortran Parallel Runtime Interface exist in [Caffeine], a parallel runtime library targeting coarray Fortran compilers. Caffeine will continue to be developed in order to fully implement the proposed Coarray Fortran Parallel Runtime Interface. Caffeine uses the [GASNet-EX] exascale networking middleware but with the library-agnostic interface and the ability to vary the communication substrate, it might also be possible to develop wrappers that would support the proposed interface with [OpenCoarrays], which uses the Message Passing Interface ([MPI]).


## Types Descriptions

 ### Fortran Intrinsic Derived types
   These types will be defined in the runtime library and it is proposed that the compiler will use a rename to use the runtime library definitions for these types in the compiler's implementation of the `ISO_Fortran_Env` module.

 #### `caf_team_type`
   * implementation for `team_type` from `ISO_Fortran_Env`
 #### `caf_event_type`
   * implementation for `event_type` from `ISO_Fortran_Env`
 #### `caf_lock_type`
   * implementation for `lock_type` from `ISO_Fortran_Env`


--------------------------------------------------------------------


 ### Runtime library specific types

 #### `caf_co_handle_t`
   * `caf_co_handle_t` will be a derived type provided by the runtime library and that will be opaque to the compiler.
 #### `caf_async_handle_t`
   * `caf_async_handle_t` will be a derived type provided by the runtime library and that will be opaque to the compiler. This type will help the runtime library track and provide asynchrony.
 #### `caf_source_loc_t`
   * `caf_source_loc_t` will be used to track the location of the critical construct blocks. The runtime library will handle critical constructs, and not expect the compiler to rewrite them as blocks with lock and unlock statements. This would be burdensome on the compiler because a lock_type variable would need to be declared, but as it needs to be a coarray, it would have to hoist its (REMOVE_NOTE: reword?!?!) declaration. REMOVE_NOTE_TODO_DECISION: The compiler will control the implementation of the type and pass it off to the runtime library OR The runtime library will control the implementation of the type and receive the required information from the compiler to create the needed instances of the type.


## Common arguments' descriptions

 #### `coarray_handle`
   * Argument for [`caf_allocate`](#caf_allocate), [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async) and all of the [atomic operations](#atomic-memory-operation)
   * scalar of type [`caf_co_handle_t`](#caf_co_handle_t)
   * This argument is a handle for the established coarray. The handle will be created when the coarray is established.
 #### `coarray_handles`
   * array of type [`caf_co_handle_t`](#caf_co_handle_t)
 #### `async_handle`
   * Argument for [`caf_get_async`](#caf_get_async), [`caf_async_wait_for`](#caf_async_wait_for), [`caf_async_try_for`](#caf_async_try_for)
   * scalar of type [`caf_async_handle_t`](#caf_async_handle_t)
   * This argument is
 #### `finished`
   * Argument for [`caf_async_try_for`](#caf_async_try_for)
   * scalar of type [`caf_async_handle_t`](#caf_async_handle_t)
   * This argument is
 #### `coindices`
   * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
   * 1d assumed-shape array of type `integer`
 #### `target`
   * assumed-rank array of `type(*)`
   * (REMOVE_NOTE: Is this note true for the puts and gets? And not just the atomics?) The location of this argument is the relevant information, not its value. This means that the compiler needs to ensure that when codegen (REMOVE_NOTE: ?) occurs, this argument is pass by reference and there is no copy made. The location of `target` is needed to compute the offset when the atomic operations' `atom` dummy argument is part of a derived type.
 #### `value`
   * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
   * assumed-rank array of `type(*)`
 #### `source`
   * Argument for [`caf_get_async`](#caf_get_async)
   * assumed-rank array of `type(*)`
 #### `team`
   * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get)
   * scalar of type `team_type`
 #### `team_number`
   * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get)
   * scalar of type `integer`
 #### `stat`
  * Argument for [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum), [`caf_put`](#caf_put), [`caf_get`](#caf_get)
  * scalar of type `integer`
  * if no error condition occurs on that image, it is assigned the value `0` (REMOVE_NOTE: ?)

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
  * **Description**: Calls to `caf_allocate` will be inserted when the compiler wants to allocate a coarray or when there is a statically declared coarray. This procedure allocates memory for a coarray. The `coarray_handle` dummy argument will pass back a handle that the runtime library will have created to be used for all future accesses and deallocation of the associated coarray. The `lbounds` and `sizes` arguments must 1d arrays with the same dimensions.
  * **Procedure Interface**:
  ```
    module subroutine caf_allocate(lbounds, sizes, coarray_handle, local_slice)
      implicit none
      type(caf_co_handle_t), intent(out) :: coarray_handle
      type(*), dimension(..), intent(inout) :: local_slice
      integer, dimension(:), intent(in) :: lbounds, sizes
    end subroutine
  ```
  REMOVE_NOTE: Fix pseudo code

 #### `caf_deallocate`
  * **Description**: This procedure
  * **Procedure Interface**:
  ```
    module subroutine caf_deallocate(coarray_handles)
      implicit none
      type(caf_co_handle_t), dimension(:), intent(out) :: coarray_handles (REMOVE_NOTE: is coarray_handles supposed to be `intent(out)`?)
    end subroutine
  ```

### Coarray Access

 Coarray accesses will maintain serial dependencies for the issuing image. A non-blocking get has to be started and finished in the same segment. The interface provides puts that are fence-based and gets that are split phased.


 #### `caf_put`
  * **Description**: Blocks on local completion. (REMOVE_NOTE: eventually would like a caf_put that doesn't block on local completion).
  * **Procedure Interface**:
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
  * **Notes**: Both optional arguments `team` and `team_number` shall not be present in the same call


 #### `caf_end_segment`
(REMOVE_NOTE): Is this procedure going to be visible to the compiler? If not, do we include discussions of it here?
  * **Description**: This procedure ends a segment. Any puts that are still in flight will be committed (and any caches will be thrown away REMOVE_NOTE_TODO_DECISION: if we decide to do caches). Calls to this procedure will be side effects of invocations of the image control statements. It is not a synchronizing operation.
  * **Procedure Interface**:
  ```
    module subroutine caf_end_segment()
      implicit none
      (REMOVE_NOTE: are there no arguments? or is it just that we haven't sketched out the args yet?)
    end subroutine
  ```

 #### `caf_get`
  * **Description**:
  * **Procedure Interface**:
  ```
    module subroutine caf_get(coarray_handle, coindices, team, team_number, source, value, stat)
      implicit none
      type(caf_co_handle_t), intent(in) :: coarray_handle
      integer, intent(in) :: coindices(:)
      type(*), dimension(..), intent(in) :: source
      type(*), dimension(..), intent(inout) :: value
      type(team_type), optional, intent(in) :: team
      integer, optional, intent(in) :: team_number
      integer, optional, intent(out) :: stat
    end subroutine
  ```
  * **Notes**: Both optional arguments `team` and `team_number` shall not be present in the same call

 #### `caf_get_async`
  * **Description**:
  * **Procedure Interface**:
  ```
    module subroutine caf_get_async(coarray_handle, coindices, team, team_number, source, value, stat, async_handle)
      implicit none
      type(caf_co_handle_t),  intent(in) :: coarray_handle
      integer, dimension(:),  intent(in) :: coindices
      type(*), dimension(..), intent(in) :: source
      type(*), dimension(..), intent(inout) :: value
      type(team_type), optional, intent(in) :: team
      integer, optional, intent(in) :: team_number
      integer, optional, intent(out) :: stat
      type(caf_async_handle_t), intent(out) :: async_handle
    end subroutine
  ```


###  Operation Synchronization

 #### `caf_async_wait_for`
  * **Description**: This procedure waits until (REMOVE_NOTE: asynchronous?) operation is complete and then consumes the async handle
  * **Procedure Interface**:
  ```
    module subroutine caf_async_wait_for(async_handle)
      implicit none
      type(caf_async_handle_t), intent(inout) :: async_handle
    end subroutine
  ```

 #### `caf_async_try_for`
  * **Description**: This procedure consumes the async handle if and only if the operation is complete
  * **Procedure Interface**:
  ```
    module subroutine caf_async_try_for(async_handle, finished)
      implicit none
      type(caf_async_handle_t), intent(inout) :: async_handle
      logical, intent(out) :: finished
    end subroutine

  ```

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

All atomic operations are blocking operations.

 #### `caf_atomic_add`
  * **Description**:
  * **Procedure Interface**: REMOVE_NOTE_TODO_DECISION:
  Option 1 with offset:
  ```
    module subroutine caf_atomic_add(coarray_handle, coindicies, offset, value, stat)
      type(caf_co_handle_t) :: coarray_handle
      integer, intent(in) :: coindices(:)
      integer :: offset, value, stat
    end subroutine
  ```

  Option 2 with target:
  ```
    module subroutine caf_atomic_add(coarray_handle, coindicies, target, value, stat)
      type(caf_co_handle_t) :: coarray_handle
      integer, intent(in) :: coindices(:) ! names image num
      integer(kind=atomic_int_kind), intent(in) :: target !location of target is relevant, not the value of target, need this to compute the offset when the `atom` dummy argument to the intrinsic is part of a derived type
      integer :: value, stat
    end subroutine
  ```

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


## Establish and initialize static coarrays prior to `main`

  (REMOVE_NOTE: complete this section, potentially move to earlier in doc) Compiler will need to: call caf_init, call caf_allocate ... for each coarray and in the right order. And then copy any initializers.


## Berkeley Lab internal Notes: (REMOVE_NOTES before submission)

  - Search for REMOVE_NOTE_TODO_DECISION to find locations where specific decisions/options are outlined, but not yet made.
  - Search for, resolve, and remove all REMOVE_NOTE and REMOVE_NOTE_TODO_DECISIONS before finalizing this document.

  * **Asynchrony:**
    -   Could be handle based or fence based approaches
    -   Handle based - return can individual operation handle, later on compiler synchronizes handle
    -   Fence based - implicit handle operations, closer to MPI



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

REMOVE_NOTEs:
    allow for non-blocking collective subroutines
    need to be able to track puts in flight, may need a write buffer, record boundaries in a hash table struct
    every single rma needs to check the table to see if there is a conflicting overlap
    could add caching
    if the constants (stat_failed_image, etc) are compiler provided, we need to get C access to these values


#### Implementation internal notes
  * In `caf_allocate`, add precondition that `lbounds` and `sizes` are the same size - use assert or other similar solution
  * In `caf_get` (and others?), `source` arg useful to get the "shape" of the thing, not the value of this dummy arg, compiler needs to ensure this dummy arg is not a copy for this strategy to work, compiler's codegen needs to ensure that this (and other subroutine calls) are not using copies for this arg
  * In `caf_get_async`, the `value` arg may need asynchronous attribute or may be implicitly asynchronous



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
