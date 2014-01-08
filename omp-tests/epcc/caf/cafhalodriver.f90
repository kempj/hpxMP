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

module cafhalodrivermod

  use cafparams
  use cafcore
  use cafhalo

  implicit none

contains

subroutine cafhalodriver

  logical :: docheck = halocheck

  integer :: iloop
  integer :: codimx, codimy, codimz
  integer :: dimx, dimy, dimz

  integer, parameter :: ndims = 3

  integer, dimension(ndims) :: codims

  call getdecomp3d(codims)

  codimx = codims(1)
  codimy = codims(2)
  codimz = codims(3)
  
  do iloop = 1, nhalosize

     dimx = halosize(iloop)

     dimy = dimx
     dimz = dimx

     if (thisimage == 1) then

        write(*,fmt='('' cafalodriver: codims = '', 2(i4,'', ''), i4)') &
             codims(1:ndims)
        write(*,fmt='('' cafalodriver: dims   = '', 2(i4,'', ''), i4)') &
             dimx, dimy, dimz

        write(*,*)

     end if

     call cafhaloswap(dimx, dimy, dimz, codimx, codimy, codimz, &
          cafsyncall, cafmodehalopp, docheck)

     call cafhaloswap(dimx, dimy, dimz, codimx, codimy, codimz, &
          cafsyncpt2pt, cafmodehalopp, docheck)

     call cafhaloswap(dimx, dimy, dimz, codimx, codimy, codimz, &
          cafsyncall, cafmodehalogg, docheck)

     call cafhaloswap(dimx, dimy, dimz, codimx, codimy, codimz, &
          cafsyncpt2pt, cafmodehalogg, docheck)

  end do

end subroutine cafhalodriver

end module cafhalodrivermod
