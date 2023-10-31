<!--===- docs/CoarrayFortranRuntime.md

   Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
   See https://llvm.org/LICENSE.txt for license information.
   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

-->
# THIS IS A WORK IN PROGRESS - DECISIONS REGARDING THE DESIGNS DISCUSSED IN THIS DOCUMENT ARE ONGOING AND MAY CHANGE AND THE DOCUMENT IS INCOMPLETE


#TODO:
   add note in an appropriate place about move_alloc when args are coarrays, say that it is the compilers responsibility to use the handles
provided from runtime library to do necessary swap


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

  The Coarray Fortran Parallel Runtime Interface is a proposed interface in which the runtime library is responsible for coarray allocation, deallocation and accesses, image synchronization, atomic operations, events, and teams. In this interface, the compiler is responsible for transforming the source code to add Fortran procedure calls to the necessary runtime library procedures. Below you can find a table showing the delegation of tasks between the compiler and the runtime library. The interface is designed for portability across shared and distributed memory machines, different operating systems, and multiple architectures. The Caffeine implementation, [see below](#caffeine-lbl's-implementation-of-the-coarray-fortran-parallel-runtime-interface), of the Coarray Fortran Parallel Runtime Interface plans to support the following architectures: x86_64, PowerPC64, AArch64, with the possibility of supporting more as requested. Implementations of this interface is intended as an augmentation for the compiler's own runtime library. While the interface can support multiple implementations, we envision needing to build the runtime library as part of installing the compiler. REMOVE_NOTE_TODO: write sentence about how a module will be defined with the procedures. name of module must be: caf_pri.

## Delegation of tasks between the Fortran compiler and the runtime library

The following table outlines which tasks will be the responsibility of the Fortran compiler and which tasks will be the responsibility of the runtime library. A '✓' in the Fortran compiler column indicates that the compiler has the primary responsibility for that task, while a '✓' in the Runtime library column indicates that the compiler will invoke the runtime library to perform the task and the runtime library has primary responsibility for the task's implementation. See the [Runtime Interface Procedures](#runtime-interface-procedures) for the list of runtime library procedures that the compiler will invoke.


| Tasks | Fortran compiler | Runtime library |
| ----  | ----- | -------- |
| Establish and initialize static coarrays prior to `main` - [see more](#establish-and-initialize-static-coarrays-prior-to-`main`)        |     ✓     |           |
| Track corank of coarrays                |     ✓     |           |
| Assigning variables of type `team-type` |     ✓     |           |
| Track locals coarrays for implicit deallocation when exiting a scope |     ✓     |           |
| Initialize a coarray with SOURCE= as part of allocate-stmt |     ✓     |           |
| Provide unique identifiers for location of each `critical-construct` |     ✓     |           |
| Provide final subroutine for all derived types with allocatable components that appear in a coarray |     ✓     |           |
| Track coarrays for implicit deallocation at `end-team-stmt`  |           |     ✓     |
| Allocate and deallocate a coarray       |           |     ✓     |
| Reference a coindexed-object            |           |     ✓     |
| Team stack abstraction                  |           |     ✓     |
| `form-team-stmt`, `change-team-stmt`, `end-team-stmt` |           |     ✓     |
| Intrinsic functions related to Coarray Fortran, like `num_images`, etc |           |     ✓     |
| Atomic subroutines                      |           |     ✓     |
| Collective subroutines                      |           |     ✓     |
| Synchronization statements              |           |     ✓     |
| Events              |           |     ✓     |
| Locks              |           |     ✓     |
| `critical-construct`             |           |     ✓     |


## Types

 **Provided Fortran types:** [`caf_event_type`](#caf_event_type), [`caf_team_type`](#caf_team_type), [`caf_lock_type`](#caf_lock_type)

 **Runtime library specific types:** [`caf_co_handle_t`](#caf_co_handle_t), [`caf_async_handle_t`](#caf_async_handle_t), [`caf_source_loc_t`](#caf_source_loc_t)

## Runtime Interface Procedures

   **Collectives:**
     [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum)

   **Program startup and shutdown:**
     [`caf_init`](#caf_init), [`caf_stop`](#caf_stop), [`caf_error_stop`](#caf_error_stop), [`caf_fail_image`](#caf_fail_image)

   **Allocation and deallocation:**
     [`caf_allocate`](#caf_allocate), [`caf_deallocate`](#caf_deallocate), [`caf_allocate_non_symmetric`](#caf_allocate_non_symmetric), [`caf_deallocate_non_symmetric`](#caf_deallocate_non_symmetric)

   **Coarray Access:**
     [`caf_put`](#caf_put), [`caf_put_raw`](#caf_put_raw), [`caf_put_raw_strided`](#caf_put_raw_strided), [`caf_get`](#caf_get), [`caf_get_raw`](#caf_get_raw), [`caf_get_raw_strided`](#caf_get_raw_strided), [`caf_get_async`](#caf_get_async), [`caf_base_pointer`](#caf_base_pointer)

   **Operation Synchronization:**
     [`caf_async_wait_for`](#caf_aync_wait_for), [`caf_async_try_for`](#caf_async_try_for), [`caf_sync_memory`](#caf_sync_memory)

   **Image Synchronization:**
     [`caf_sync_all`](#caf_sync_all), [`caf_sync_images`](#caf_sync_images), [`caf_lock`](#caf_lock), [`caf_unlock`](#caf_unlock), [`caf_critical`](#caf_critical), [`caf_end_critical`](#caf_end_critical)

   **Events:**
     [`caf_event_post`](#caf_event_post), [`caf_event_wait`](#caf_event_wait), [`caf_event_query`](#caf_event_query)

   **Teams:**
     [`caf_change_team`](#caf_change_team), [`caf_end_team`](#caf_end_team), [`caf_form_team`](#caf_form_team), [`caf_sync_team`](#caf_sync_team), [`caf_get_team`](#caf_get_team), [`caf_team_number`](#caf_team_number), [`caf_alias_create`](#caf_alias_create), [`caf_alias_destroy`](#caf_alias_destroy)

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
   These types will be defined in the runtime library and it is proposed that the compiler will use a rename to use the runtime library definitions for these types in the compiler's implementation of the `ISO_Fortran_Env` module. REMOVE_NOTE_TODO: add rationale for this

 #### `caf_team_type`
   * implementation for `team_type` from `ISO_Fortran_Env`
 #### `caf_event_type`
   * implementation for `event_type` from `ISO_Fortran_Env`
 #### `caf_lock_type`
   * implementation for `lock_type` from `ISO_Fortran_Env`


 ### Runtime library specific types
   REMOVE_NOTE_TODO:    ADD general description of types

 #### `caf_co_handle_t`
   * `caf_co_handle_t` will be a derived type provided by the runtime library and that will be opaque to the compiler.
 #### `caf_async_handle_t`
   * `caf_async_handle_t` will be a derived type provided by the runtime library and that will be opaque to the compiler. This type will help the runtime library track and provide asynchrony.


## Procedure descriptions

### sync-stat-list

  * **`stat`** : TODO
  * **`errmsg`** : There will be two optional arguments for this, one which is allocatable and one which is not. It is the compiler's responsibility to ensure the appropriate optional argument is passed. The allocatable argument will satisfy Fortan 2023 semantics.

### Collectives

 #### Common arguments
  * **`a`**
    * Argument for all the collective subroutines: [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum),
    * may be any type
    * is always `intent(inout)`
    * for [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum) it is assigned the value computed by the collective operation, if no error conditions occurs and if `result_image` is absent, or the executing image is the one identified by `result_image`, otherwise `a` becomes undefined
    * for [`co_broadcast`](#co_broadcast), the value of the argument on the `source_image` is assigned to the `a` argument on all other images

  * **`stat`**
    * Argument for all the collective subroutines: [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum),
    * is always of type `integer`
    * is always `intent(out)`
    * is assigned the value `0` when the execution of the procedure is succcessful
    * is assigned a positive value when the execution of the procedure is not succcessful and the `a` argument becomes undefined

  * **`errmsg`**
    * Argument for all the collective subroutines: [`caf_co_broadcast`](#caf_co_broadcast), [`caf_co_max`](#caf_co_max), [`caf_co_min`](#caf_co_min), [`caf_co_reduce`](#caf_co_reduce), [`caf_co_sum`](#caf_co_sum),
    * is always of type `integer`
    * is always `intent(inout)`
    * if an error condition does not occur, the value is unchanged
    * if an error condition occurs, an explanatory message is assigned to the argument


  (REMOVE_NOTE_TODO: check the interfaces for these collectives, currently are same as the procedures in Caffeine, but these interfaces have not yet been discussed and decided upon for the Coarray Fortran Parallel Runtime Library Interface. May need to add something?)

 #### `caf_co_broadcast`
  * **Description**:
  * **Procedure Interface**:
    ```
       subroutine caf_co_broadcast(a, source_image, stat, errmsg)
         implicit none
         type(*), intent(inout), contiguous, target :: a(..)
         integer, optional, intent(in) :: source_image
         integer, optional, intent(out), target :: stat
         character(len=*), intent(inout), optional, target :: errmsg
       end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_co_max`
  * **Description**:
  * **Procedure Interface**:
    ```
       subroutine caf_co_max(a, result_image, stat, errmsg)
         implicit none
         type(*), intent(inout), contiguous, target :: a(..)
         integer, intent(in), optional, target :: result_image
         integer, intent(out), optional, target :: stat
         character(len=*), intent(inout), optional, target :: errmsg
       end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_co_min`
  * **Description**:
  * **Procedure Interface**:
    ```
       subroutine caf_co_min(a, result_image, stat, errmsg)
         implicit none
         type(*), intent(inout), contiguous, target :: a(..)
         integer, intent(in), optional, target :: result_image
         integer, intent(out), optional, target :: stat
         character(len=*), intent(inout), optional, target :: errmsg
       end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_co_reduce`
  * **Description**:
  * **Procedure Interface**:
    ```
       subroutine caf_co_reduce(a, operation, result_image, stat, errmsg)
         implicit none
         type(*), intent(inout), contiguous, target :: a(..)
         type(c_funptr), value :: operation
         integer, intent(in), optional, target :: result_image
         integer, intent(out), optional, target :: stat
         character(len=*), intent(inout), optional, target :: errmsg
       end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_co_sum`
  * **Description**:
  * **Procedure Interface**:
    ```
       subroutine caf_co_sum(a, result_image, stat, errmsg)
         implicit none
         type(*), intent(inout), contiguous, target :: a(..)
         integer, intent(in), target, optional :: result_image
         integer, intent(out), target, optional :: stat
         character(len=*), intent(inout), target, optional :: errmsg
       end subroutine
    ```
  * **Further argument descriptions**:

### Program startup and shutdown

  When the compiler identifies a program that uses "Coarray Fortran" features, it will insert calls to `caf_init` and `caf_stop`. These procedures will initialize and terminate the Coarray Fortran environment.

 #### `caf_init`
  * **Description**: This procedure will initialize the Coarray Fortran environment.
  * **Procedure Interface**:
    ```
      function caf_init() result(exit_code)
        implicit none
        integer :: exit_code
      end function
    ```
  * **Result**: `exit_code` is an `integer` whose value ... (REMOVE_NOTE_TODO: fill in)

REMOVE_NOTE_TODO: resolve the following two options for caf_stop
 #### `caf_stop`
  * **Description**: This procedure may or may terminate the Coarray Fortran environment. REMOVE_NOTE_TODO_DECISION: does it terminate for sure or not? can caf_init be called twice?
  * **Procedure Interface**:
    ```
      subroutine caf_stop(exit_code)
        implicit none
        integer, intent(in) :: exit_code
      end subroutine
    ```
  * **Further argument descriptions**:
    * **`exit_code`**: is .. (REMOVE_NOTE_TODO: fill in)

  (REMOVE_NOTE_TODO: check the interfaces for caf_error_stop and caf_stop, currently are same as the procedures in Caffeine, but these interfaces have not yet been discussed and decided upon for the Coarray Fortran Parallel Runtime Library Interface. May need to add something? Change something?)

  * **Description**: This procedure synchronizes and stops the executing image. It provides the `stop_code` or `0` if no `stop_code` is passed, as the process exit status.
  * **Procedure Interface**:  REMOVE_NOTE_TODO_DECISION: should stop be implemented with two optional arguments with the precondition that they both shall not be passed at the same time? Or have overloaded procedures?
    ```
      subroutine caf_stop(stop_code_int, stop_code_char)
        implicit none
        integer, intent(in), optional :: stop_code_int
        character(len=*), intent(in), optional :: stop_code_char
      end subroutine

    ```
  * **Further argument descriptions**:
    * **`stop_code_int` and `stop_code_char`**: shall not both be present in the same call (if provide only one procedure instead of overloading caf_stop)
          * In `caf_stop`, runtime library will need to call c_exit() REMOVE_NOTE_TODO: fix this note

 #### `caf_error_stop`
  * **Description**: This procedure stops all images and provides the `stop_code` passed, or `0` if no `stop_code` is passed, as the process exit status
  * **Procedure Interface**: REMOVE_NOTE_TODO_DECISION: should error_stop be implemented with two optional arguments with the precondition that they both shall not be passed at the same time? Or have overloaded procedures?
    ```
      subroutine caf_error_stop(stop_code_int, stop_code_char)
        integer, intent(in), optional :: stop_code_int
        character(len=*), intent(in), optional :: stop_code_char
      end subroutine
    ```
  * **Further argument descriptions**:
    * **`stop_code_int` and `stop_code_char`**: shall not both be present in the same call (if provide only one procedure instead of overloading caf_error_stop)

 #### `caf_fail_image`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_fail_image(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

### Allocation and deallocation

 #### `caf_allocate`
  * **Description**: This procedure allocates memory for a coarray. Calls to `caf_allocate` will be inserted by the compiler when there is an explicit coarray allocation or a statically declared coarray in the source code. The runtime library will stash away the coshape information at this time in order to internally track it during the lifetime of the coarray.
  * **Procedure Interface**:
    ```
      subroutine caf_allocate(lcobounds, ucobounds, lbounds, ubounds, element_length, final_func, coarray_handle, allocated_memory)
        implicit none
        integer(kind=c_intmax_t), dimension(:), intent(in) :: lcobounds, ucobounds
        integer(kind=c_intmax_t), dimension(:), intent(in) :: lbounds, ubounds
        integer(kind=c_size_t) :: element_length
        type(c_funptr), intent(in) :: final_func
        type(caf_co_handle_t), intent(out) :: coarray_handle
        type(c_ptr), intent(out) :: allocated_memory
      end subroutine
    ```
  * **Further argument descriptions**:
    * **`lcobounds` and `ucobounds`**: Shall be the lower and upper bounds of the coarray being allocated. Shall be 1d arrays with the same dimensions as each other. The product of the difference of the `lcobounds` and `ucobounds` shall equal the number of team members (REMOVE_NOTE_TODO: check wording).
    * **`lbounds` and `ubounds`**: Shall be the the lower and upper bounds of the `local_slice`. Shall be 1d arrays with the same dimensions as each other.
    * **`element_length`**: Length of the element
    * **`final_func`**: Shall be a function pointer to the final function, if any, for derived types
    * **`coarray_handle`**: Represents the distributed object of the coarray on the corresponding team. Shall return the handle created by the runtime library that the compiler shall use for future coindexed-object references of the associated coarray and for deallocation of the associated coarray.
    * **`allocated_memory`**: Shall not be allocated on entry. Shall return a pointer to the block of allocated memory for the Fortran object.

 #### `caf_allocate_non_symmetric`
  * **Description**: This procedure allocates components of coarray objects. If the object to be allocated is polymorphic, it is the compiler's responsibility to set the dynamic type and it shall not be accessed remotely.
  * **Procedure Interface**:
    ```
     module subroutine caf_allocate_non_symmetric(size_in_bytes, allocated_memory)
       implicit none
       integer(kind=c_size_t) :: size_in_bytes
       type(c_ptr), intent(out) :: allocated_memory
     end subroutine
    ```
  * **Further argument descriptions**:
    * **`size_in_bytes`**: The size, in bytes, of the object to be allocated.
    * **`allocated_memory`**: Shall not be allocated on entry. Shall return a pointer to the block of allocated memory for the Fortran object.

 #### `caf_deallocate`
  * **Description**: This procedure releases memory previously allocated for all of the coarrays associated with the handles in `coarray_handles`, resulting in the destruction of any associated `local_slices` received by the compiler after `caf_allocate` calls.  (REMOVE_NOTE_TODO: reword) The compiler will insert calls to this procedure when exiting a local scope where implicit deallocation of a coarray is mandated by the standard and when a coarray is explicitly deallocated through a `deallocate-stmt` in the source code.
  * **Procedure Interface**:
    ```
      subroutine caf_deallocate(coarray_handles)
        implicit none
        type(caf_co_handle_t), dimension(:), intent(in) :: coarray_handles
      end subroutine
    ```
  * **Argument descriptions**:
    * **`coarray_handles`**: Is an array of all of the handles for the coarrays that shall be deallocated.

 #### `caf_deallocate_non_symmetric`
  * **Description**: This procedure releases memory previously allocated for a component of a derived type coarray.
  * **Procedure Interface**:
    ```
      subroutine caf_deallocate_non_symmetric(mem)
        implicit none
        type(c_ptr), intent(in) :: mem
      end subroutine
    ```
  * **Argument descriptions**:
    * **`mem`**: Pointer to the block of memory to be released.


### Coarray Access

 Coarray accesses will maintain serial dependencies for the issuing image. A non-blocking get has to be started and finished in the same segment. The interface provides puts that are fence-based and gets that are split phased.

 #### Common arguments
  * **`coarray_handle`**
    * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
    * scalar of type [`caf_co_handle_t`](#caf_co_handle_t)
    * is a handle for the established coarray
    * represents the distributed object of the coarray on the corresponding team
  * **`coindices`**
    * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
    * 1d assumed-shape array of type `integer`
    * (REMOVE_NOTE_TODO: fill in)
  * **`value`**
    * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
    * assumed-rank array of `type(*)`
    * is (REMOVE_NOTE_TODO: fill in)
  * **`mold`**
    * Argument for [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
    * assumed-rank array of `type(*)`
    * is (REMOVE_NOTE_TODO: fill in)
  * **`team` and `team_number`**
    * Argument for [`caf_put`](#caf_put), [`caf_get`](#caf_get), [`caf_get_async`](#caf_get_async)
    * are optional arguments that specify a team
    * shall not both be present in the same call

 #### `caf_put`
  * **Description**: This procedure assigns to a coarray, when both sides of the assignment are contiguous. The compiler shall call this procedure when there is a coarray reference that is a `coindexed-object`. The compiler shall not (REMOVE_NOTE: need to?) call this procedure when the coarray reference is not a `coindexed-object`. This procedure blocks on local completion. (REMOVE_NOTE: eventually would like a caf_put that doesn't block on local completion).
  * **Procedure Interface**:
    ```
     subroutine caf_put(coarray_handle, coindices, value, element_storage_size, first_element_addr, team, team_number, stat)
       implicit none
       type(caf_co_handle_t), intent(in) :: coarray_handle
       integer, intent(in) :: coindices(:)
       type(*), dimension(..), intent(in), contiguous :: value
       integer(kind=c_size_t), intent(in) :: element_storage_size
       type(c_ptr), intent(in) :: first_element_addr ! represents the address in the local slice corresponding to where the first element lives on the remote
       type(team_type), optional, intent(in) :: team
       integer, optional, intent(in) :: team_number
       integer, optional, intent(out) :: stat
     end subroutine caf_put
    ```
  * **Further argument descriptions**:
    * **`value`**: The value that shall be assigned to (REMOVE_NOTE_TODO: fill in)


 #### `caf_put_strided`
  * **Description**: TODO: fill in
  * **Procedure Interface**:
    ```
     ! more general case
     subroutine caf_put_strided( &
         coarray_handle, coindices, value, element_storage_size, &
         first_element_addr, extent, stride, team, team_number, stat)
       implicit none
       type(caf_co_handle_t), intent(in) :: coarray_handle
       integer, intent(in) :: coindices(:)
       type(*), dimension(..), intent(in) :: value
       integer(kind=c_size_t), intent(in) :: element_storage_size
       type(c_ptr), intent(in) :: first_element_addr ! represents the address in the local slice corresponding to where the first element lives on the remote
       integer(kind=c_size_t) :: extent(:)
       integer(kind=c_ptrdiff_t) :: stride(:)
       type(team_type), optional, intent(in) :: team
       integer, optional, intent(in) :: team_number
       integer, optional, intent(out) :: stat
     end subroutine
    ```
  * **Further argument descriptions**:
    * **`coarray_handle`**:

 #### `caf_put_raw`
  * **Description**: Assign to size number of bytes from image named by coindicies starting at remote pointer, copying from local_buffer.
  * **Procedure Interface**:
    ```
      subroutine caf_put_raw(image_num, local_buffer, remote_ptr, size, stat)
        implicit none
        integer(kind=c_int), intent(in) :: image_num
        type(c_ptr), intent(in) :: local_buffer
        integer(kind=c_int64_t), intent(in) :: remote_ptr
        integer(kind=c_size_t), intent(in) :: size
        integer, optional, intent(out) :: stat
      end subroutine
    ```

 #### `caf_put_raw_strided`
  * **Description**: TODO: (reword) Assign to size number of bytes from image named by coindicies starting at remote pointer, use stride to assign, copying from local_buffer.
  * **Procedure Interface**:
    ```
       subroutine caf_put_raw_strided(image_num, local_buffer, remote_ptr, size, extent, remote_ptr_stride, local_buffer_stride, stat)
        implicit none
        integer(kind=c_int), intent(in) :: image_num
        type(c_ptr), intent(in) :: local_buffer
        integer(kind=c_int64_t), intent(in) :: remote_ptr
        integer(kind=c_size_t), intent(in) :: size
        integer(kind=c_size_t) :: extent(:)
        integer(kind=c_ptrdiff_t) :: remote_ptr_stride(:), local_buffer_stride(:)
        integer, optional, intent(out) :: stat
      end subroutine
    ```

 #### `caf_get`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_get(coarray_handle, coindices, mold, value, team, team_number, stat)
        implicit none
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer, intent(in) :: coindices(:)
        type(*), dimension(..), intent(in) :: mold
        type(*), dimension(..), intent(inout) :: value
        type(team_type), optional, intent(in) :: team
        integer, optional, intent(in) :: team_number
        integer, optional, intent(out) :: stat
      end subroutine
    ```

 #### `caf_get_async`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_get_async(coarray_handle, coindices, mold, value, async_handle, team, team_number, stat)
        implicit none
        type(caf_co_handle_t),  intent(in) :: coarray_handle
        integer, dimension(:),  intent(in) :: coindices
        type(*), dimension(..), intent(in) :: mold
        type(*), dimension(..), intent(inout) :: value
        type(caf_async_handle_t), intent(out) :: async_handle
        type(team_type), optional, intent(in) :: team
        integer, optional, intent(in) :: team_number
        integer, optional, intent(out) :: stat
      end subroutine
    ```

 #### `caf_get_raw`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_get_raw(image_num, local_buffer, remote_ptr, size, stat)
        implicit none
        integer(kind=c_int), intent(in) :: image_num
        type(c_ptr), intent(in) :: local_buffer
        integer(kind=c_int64_t), intent(in) :: remote_ptr
        integer(kind=c_size_t), intent(in) :: size
        integer, optional, intent(out) :: stat
      end subroutine
    ```

 #### `caf_get_raw_strided`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_get_raw_strided(image_num, local_buffer, remote_ptr, size, extent, remote_ptr_stride, local_buffer_stride, stat)
        implicit none
        integer(kind=c_int), intent(in) :: image_num
        type(c_ptr), intent(in) :: local_buffer
        integer(kind=c_int64_t), intent(in) :: remote_ptr
        integer(kind=c_size_t), intent(in) :: size
        integer(kind=c_size_t) :: extent(:)
        integer(kind=c_ptrdiff_t) :: remote_ptr_stride(:), local_buffer_stride(:)
        integer, optional, intent(out) :: stat
      end subroutine
    ```

 #### `caf_base_pointer`
  * **Description**: This procedure provides a pointer to the base of the coarray elements on a given image and may be used in conjunction with caf_get_raw
  * **Procedure Interface**:
    ```
      function caf_base_pointer(coarray_handle, coindices, team, team_number, stat) result (raw_ptr_int)
        implicit none
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer, intent(in) :: coindices(:)
        integer(kind=c_int64_t) :: raw_ptr_int
        type(team_type), optional, intent(in) :: team
        integer, optional, intent(in) :: team_number
        integer, optional, intent(out) :: stat
      end function
    ```

###  Operation Synchronization


 #### Common arguments
  * **`async_handle`**
    * Argument for [`caf_async_wait_for`](#caf_async_wait_for), [`caf_async_try_for`](#caf_async_try_for)
    * scalar of type [`caf_async_handle_t`](#caf_async_handle_t)
    * This argument is a handle used to track the asynchronous operation REMOVE_NOTE_TODO: reword and buff out this sentence


 #### `caf_async_wait_for`
  * **Description**: This procedure waits until (REMOVE_NOTE: asynchronous?) operation is complete and then consumes the async handle
  * **Procedure Interface**:
    ```
      subroutine caf_async_wait_for(async_handle)
        implicit none
        type(caf_async_handle_t), intent(inout) :: async_handle
      end subroutine
    ```

 #### `caf_async_try_for`
  * **Description**: This procedure consumes the async handle if and only if the operation is complete
  * **Procedure Interface**:
    ```
      subroutine caf_async_try_for(async_handle, finished)
        implicit none
        type(caf_async_handle_t), intent(inout) :: async_handle
        logical, intent(out) :: finished
      end subroutine
    ```
  * **Further argument descriptions**:
    * **`finished`**: This argument returns `true` if the asynchronous operation is complete

 #### `caf_sync_memory`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_sync_memory(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

### Image Synchronization

  Compilers need an "optimization fence",
  inline assembly available in c/c++ to provide optimization fences

 #### `caf_sync_all`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_sync_all(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_sync_images`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_sync_images(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_lock`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_lock(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_unlock`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_unlock(fill in...)
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_critical`
  * **Description**: For each critical construct, the compiler shall define a coarray that shall only be used to begin and end the critical block. The coarray shall be a scalar coarray of type `lock_type` and the associated coarray handle shall be passed to the procedure.
  * **Procedure Interface**:
    ```
      subroutine caf_critical(critical_coarray, stat)
        implicit none
        type(caf_co_handle_t), intent(in) :: critical_coarray
        integer, optional, intent(out) :: stat
      end subroutine
    ```
  * **Further argument descriptions**:
    * **`critical_coarray`**:

#### `caf_end_critical`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_end_critical(critical_coarray)
        implicit none
        type(caf_co_handle_t), intent(in) :: critical_coarray
      end subroutine
    ```
  * **Further argument descriptions**:
      * **`critical_id`**: shall be the same unique identifier for the critical construct that was passed to the runtime library during the corresponding call to `caf_critical`. REMOVE_NOTE_TODO: reword?

### Events

 #### `caf_event_post`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_event_post(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_event_wait`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_event_wait(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_event_query`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_event_query(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

### Teams
  TODO: add general points related to our team design
  The only time we create a new handle for an established coarray is in a change-team-stmt. The only times we create handles is `caf_allocate`, and a `change-team-stmt`.

 #### `caf_change_team`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_change_team(team_var, errmsg, stat)
        implicit none
        type(caf_team_type_t), intent(in) :: team_var
        character(len=*), intent(out), optional :: errmsg
        integer, intent(out), optional :: stat
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_end_team`
  * **Description**: During the execution of `caf_end_team`, the runtime library will implicitly synchronize, as the standard requires and deallocate any coarrays allocated during the change team construct. Prior to `caf_end_team`, the compiler is responsible for invoking [`caf_alias_destroy`](#caf_alias_destroy) for any `caf_co_handle_t` handles created during the change team construct.
  * **Procedure Interface**:
    ```
      subroutine caf_end_team(errmsg, stat)
        implicit none
        character(len=*), intent(out), optional :: errmsg
        integer, intent(out), optional :: stat
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_form_team`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_form_team(team_num, team_var, new_index, errmsg, stat)
        implicit none
        integer(kind=c_int), intent(in) :: team_num
        type(caf_team_type_t), intent(out) :: team_var
        integer(kind=c_int), intent(in), optional :: new_index
        character(len=*), intent(out), optional :: errmsg
        integer, intent(out), optional :: stat
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_sync_team`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_sync_team(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_get_team`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_get_team(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_team_number`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_team_number(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_alias_create`
  * **Description**: Create a new coarray handle that may be used for coarray queries about an aliased coarray, such as in a [`caf_change_team`](#caf_change_team)
  * **Procedure Interface**:
    ```
      subroutine caf_alias_create(source_handle, alias_co_lbounds, alias_co_ubounds, alias_handle)
        implicit none
        type(caf_co_handle_t), intent(in) :: source_handle
        integer(kind=c_intmax_t), dimension(:), intent(in) :: alias_co_lbounds, alias_co_ubounds
        type(caf_co_handle_t), intent(out) :: alias_handle
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_alias_destroy`
  * **Description**: REMOVE_NOTE_TODO: Keep the alias routines in the Teams section? Move to another more general section?
  * **Procedure Interface**:
    ```
      subroutine caf_alias_destroy(alias_handle)
        implicit none
        type(caf_co_handle_t), intent(in) :: alias_handle
      end subroutine
    ```
  * **Further argument descriptions**:

### Atomic Memory Operation

All atomic operations are blocking operations.

 #### Common arguments
  * **`target`**
    * Argument for all of the atomics (REMOVE_NOTE_TODO_DECISION: have we decided to deal with atomics with the offset option or the target option?)
    * assumed-rank array of `type(*)`
    * The location of this argument is the relevant information, not its value. As such, the compiler needs to ensure that when codegen (REMOVE_NOTE: ?) occurs, this argument is pass by reference and there is no copy made. The location of `target` is needed to compute the offset when the atomic operations' `atom` dummy argument is part of a derived type.


 #### `caf_atomic_add`
  * **Description**:
  * **Procedure Interface**: REMOVE_NOTE_TODO_DECISION:
  Option 1 with offset:
    ```
      subroutine caf_atomic_add(coarray_handle, coindicies, offset, value, stat)
        implicit none
        type(caf_co_handle_t) :: coarray_handle
        integer, intent(in) :: coindices(:)
        integer :: offset, value, stat
      end subroutine
    ```

  Option 2 with target:
    ```
      subroutine caf_atomic_add(coarray_handle, coindicies, target, value, stat)
        implicit none
        type(caf_co_handle_t) :: coarray_handle
        integer, intent(in) :: coindices(:) ! names image num
        integer(kind=atomic_int_kind), intent(in) :: target !location of target is relevant, not the value of target, need this to compute the offset when the `atom` dummy argument to the intrinsic is part of a derived type
        integer :: value, stat
      end subroutine
    ```

 #### `caf_atomic_and`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_and(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_cas`
  * **Description**:
  * **Procedure Interfaces**:
    ```
      subroutine caf_atomic_cas_int_raw(image_num, atom_remote_ptr, old, compare, new, stat)
        implicit none
        integer, intent(in) :: image_num
        integer(kind=c_int64_t), intent(in) :: atom_remote_ptr
        integer(kind=atomic_int_kind), intent(in)  :: compare, new
        integer(kind=atomic_int_kind), intent(out) :: old
        integer, optional, intent(out) :: stat
      end subroutine

      subroutine caf_atomic_cas_logical_raw(image_num, atom_remote_ptr, old, compare, new, stat)
        implicit none
        integer, intent(in) :: image_num
        integer(kind=c_int64_t), intent(in) :: atom_remote_ptr
        logical(kind=atomic_logical_kind), intent(in)  :: compare, new
        logical(kind=atomic_logical_kind), intent(out) :: old
        integer, optional, intent(out) :: stat
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_define`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_define(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_fetch_add`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_fetch_add_raw(image_num, atom_remote_ptr, value, old, stat)
        implicit none
        integer, intent(in) :: image_num
        integer(kind=c_int64_t), intent(in) :: atom_remote_ptr
        integer(kind=atomic_int_kind), intent(in)  :: value
        integer(kind=atomic_int_kind), intent(out) :: old
        integer, optional, intent(out) :: stat
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_fetch_and`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_fetch_and(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_fetch_or`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_fetch_or(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_fetch_xor`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_fetch_xor(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_or`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_or(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_ref`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_ref(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_atomic_xor`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_atomic_xor(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

### Coarray Queries

 #### `caf_lcobound`
  * **Description**: This procedure returns the lcobounds of the coarray referred to by the coarray_handle. The lcobounds will always be a 64bit integer and it is the compiler's responsibility to convert to a different kind, as needed by the user.
  * **Procedure Interface**:
    ```
      interface caf_lcobound
        module procedure caf_lcobound_with_dim
        module procedure caf_lcobound_no_dim
      end interface caf_lcobound

      subroutine caf_lcobound_with_dim(coarray_handle, dim, lcobound)
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer, intent(in) :: dim
        integer(kind=c_int64_t), intent(out) :: lcobound
      end subroutine

      subroutine caf_lcobound_no_dim(coarray_handle, lcobounds)
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer(kind=c_int64_t), intent(out) :: lcobounds(:)
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_ucobound`
  * **Description**: This procedure returns the ucobounds of the coarray referred to by the coarray_handle. The ucobounds will always be a 64bit integer and it is the compiler's responsibility to convert to a different kind, as needed by the user.

  * **Procedure Interface**:
    ```
      interface caf_ucobound
        module procedure caf_ucobound_with_dim
        module procedure caf_ucobound_no_dim
      end interface

      subroutine caf_ucobound_with_dim(coarray_handle, dim, ucobound)
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer, intent(in) :: dim
        integer(kind=c_int64_t), intent(out) :: ucobound
      end subroutine

      subroutine caf_ucobound_no_dim(coarray_handle, ucobounds)
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer(kind=c_int64_t), intent(out) :: ucobounds(:)
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_coshape`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_coshape(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_image_index`
  * **Description**:
  * **Procedure Interface**:
    ```
      function caf_image_index(coarray_handle, coindices)
        implicit none
        type(caf_co_handle_t), intent(in) :: coarray_handle
        integer, intent(in) :: coindices(:)
        integer(kind=c_int) :: caf_image_index
      end function
    ```
  * **Further argument descriptions**:

### Image Queries

 #### `caf_num_images`
  * **Description**:
  * **Procedure Interface**:   (REMOVE_NOTE_TODO: check the interface for caf_num_images, currently is same as the procedure in Caffeine, but this interface has not yet been discussed and decided upon for the Coarray Fortran Parallel Runtime Library Interface. May need to add something? Change something?)
    ```
      function caf_num_images(team, team_number) result(image_count)
        implicit none
        type(team_type), intent(in), optional :: team
        integer, intent(in), optional :: team_number
        integer :: image_count
      end function
    ```
  * **Further argument descriptions**:
    * **`team` and `team_number`**: optional arguments that specify a team. They shall not both be present in the same call.
  * **Result**:

 #### `caf_this_image`
  * **Description**:
  * **Procedure Interface**:
    ```
      function caf_this_image(fill in...)
        implicit none
      end function
    ```
  * **Further argument descriptions**:

 #### `caf_failed_images`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_failed_images(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_stopped_images`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_stopped_images(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:

 #### `caf_image_status`
  * **Description**:
  * **Procedure Interface**:
    ```
      subroutine caf_image_status(fill in...)
        implicit none
      end subroutine
    ```
  * **Further argument descriptions**:


## Establish and initialize static coarrays prior to `main`

  (REMOVE_NOTE: complete this section, potentially move to earlier in doc) Compiler will need to: call caf_init, call caf_allocate ... for each coarray and in the right order. And then copy any initializers.

# Testing plan
[tbd]

[Caffeine]: https://go.lbl.gov/caffeine
[GASNet-EX]: https://go.lbl.gov/gasnet
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[MPI]: https://www.mpi-forum.org
[Rouson and Bonachea (2022)]: https://doi.org/10.25344/S4459B
