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

module cafsyncdrivermod

  use cafparams
  use cafcore
  use cafsync

  implicit none

contains

subroutine cafsyncdriver()

  integer :: ineigh


  call cafsyncbench(syncouter, syncinner, syncdelay, cafsyncall,  1)
#ifdef MPI
  call cafsyncbench(syncouter, syncinner, syncdelay, cafsyncmpi,  1)
#endif

  call cafsyncbench(syncouter, syncinner, syncdelay, cafsyncpair, 1)


  do ineigh = 2, min(syncmaxneigh, numimages-2), 2

     call cafsyncbench(syncouter, syncinner, syncdelay, cafsyncring, ineigh)
     call cafsyncbench(syncouter, syncinner, syncdelay, cafsyncrand, ineigh)

  end do

  if (numimages .ge. 8) then

     call cafsyncbench(syncouter, syncinner, syncdelay, cafsync3d, 6)

  end if

  call cafsyncbench(syncouter, syncinner, syncdelay, cafsynclock, 1)
  call cafsyncbench(syncouter, syncinner, syncdelay, cafsynccrit, 1)

end subroutine cafsyncdriver

end module cafsyncdrivermod
