!----------------------------------------------------------------------------!
!                                                                            !
!  Fortran Coarray MicroBenchmark Suite - Version 0.9                        !
!                                                                            !
!  David Henty, EPCC; d.henty@epcc.ed.ac.uk                                  !
!                                                                            !
!  Copyright 2012 the University of Edinburgh                                !
!                                                                            !
!  Licensed under the Apache License, Version 2.0 (the "License");           !
!  you may not use this file except in compliance with the License.          !
!  You may obtain a copy of the License at                                   !
!                                                                            !
!      http://www.apache.org/licenses/LICENSE-2.0                            !
!                                                                            !
!  Unless required by applicable law or agreed to in writing, software       !
!  distributed under the License is distributed on an "AS IS" BASIS,         !
!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  !
!  See the License for the specific language governing permissions and       !
!  limitations under the License.                                            !
!                                                                            !
!----------------------------------------------------------------------------!

module cafparams

  implicit none

! Global parameters
! -----------------
!
! p2pbench:  whether or not to perform the point-to-point benchmarks
! syncbench: whether or not to perform the synchronisation benchmarks
! halobench: whether or not to perform the halo swapping benchmarks
!

  logical, parameter :: p2pbench  = .true.
  logical, parameter :: syncbench = .true.
  logical, parameter :: halobench = .true.


! Individual configuration parameters for each of the three benchmarks
! --------------------------------------------------------------------
!

! Point-to-point parameters
! -------------------------
!
! p2pcheck: do we verify the results (impacts performance but should be
!           done occasionally as compilers can go wrong!)
!
! p2psingle: perform ping-pong between first and last image
! p2pmulti:  perform ping-pong between all images (in pairs)
! p2pcross:  as above but half the pairs send as the other half receive
!
! p2pnmax     : maximum array size for all access patterns
! p2pmaxstride: maximum stride for strided access patterns
!
! p2ptargettime: minimum acceptable time for a test (the code self-adjusts)
!

  logical, parameter :: p2pcheck = .false.

  logical, parameter :: p2psingle = .true.
  logical, parameter :: p2pmulti  = .true.
  logical, parameter :: p2pcross  = .false.

  integer, parameter :: p2pnmax      = 4*1024*1024
  integer, parameter :: p2pmaxstride = 128

  double precision, parameter :: p2ptargettime = 0.5


! Synchronisation parameters
! --------------------------
!
! syncouter: number of times each test is run
! syncinner: number of repetitions for each test
! syncdelay: controls the length of the delay loop
!
! syncmaxneigh: maximum neighbours for each point-to-point pattern
!

  integer, parameter :: syncouter =   100
  integer, parameter :: syncinner =   200
  integer, parameter :: syncdelay = 10000

  integer, parameter :: syncmaxneigh = 12


! Halo swapping  parameters
! -------------------------
!
! halocheck: do we verify the results (impacts performance but should be
!            done occasionally as compilers can go wrong!)
!
! nhalosize: how many different array sizes are tested
! halosize:  array sizes (default is cubic although actual code is general)
!
! halotargettime: minimum acceptable time for a test (the code self-adjusts)
!

  logical, parameter :: halocheck = .false.

  integer, parameter            :: nhalosize = 4
  integer, dimension(nhalosize) :: halosize = [10, 50, 100, 150]

  double precision, parameter :: halotargettime = 0.5

end module cafparams
