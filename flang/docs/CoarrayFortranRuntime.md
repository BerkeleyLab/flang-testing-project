<!--===- docs/CoarrayFortranRuntime.md

   Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
   See https://llvm.org/LICENSE.txt for license information.
   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

-->

# Problem description
  In order to be fully Fortran 2018 compliant, Flang needs to add support for what is commonly referred to as coarray fortran,
  which includes features related to parallelism. These features include coarrays, teams, events, and the statements, intrinsic
  subroutines and functions that support them. These statements include `sync-all-stmt`, `sync-images-stmt`, `sync-memory-stmt`,
  `sync-team-stmt`, `event-post-stmt`, `event-wait-stmt`,` error-stop-stmt`, `lock-stmt`. The intrinsic functions include
  `num_images`, `this_image`, `lcobound`, `ucobound`, `team_number`, `get_team`, `failed_images`, `stopped_images`,
  `image_status`, `coshape`, `image_index`. The intrinsic subroutines include `event_query`, the collectives: `co_sum`, `co_max`,
  `co_min`, `co_reduce`, `co_broadcast` and the atomics: `atomic_add`, `atomic_and`, `atomic_cas`, `atomic_define`,
  `atomic_fetch_add`, `atomic_fetch_and`, `atomic_fetch_or`, `atomic_fetch_xor`, `atomic_or`, `atomic_ref`, `atomic_xor`.



# Proposed solution
  This design document proposes to use [Caffeine](https://github.com/berkeleylab/caffeine), a parallel runtime library, to handle the runtime support for coarray Fortran features.



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
| Track when implicit coarray deallocation needs to occur when exiting a scope |     ✓     |           |
| Implementing the intrinsic `coshape`    |     ?     |     ?     |
| Team stack abstraction                  |           |     ✓     |
| `form-team-stmt`                        |           |     ✓     |
| `change-team-stmt`                      |           |     ✓     |
| `end-team-stmt`                         |           |     ✓     |
| Allocate a coarray                      |           |     ✓     |
| Deallocate a coarray                    |           |     ✓     |
| Reference a coarray                     |           |     ✓     |



# Testing plan
[tbd]
