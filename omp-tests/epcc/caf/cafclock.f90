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

module cafclock

  implicit none

contains

  double precision function caftime()

    logical, save :: firstcall = .true.

    integer, parameter :: int32kind = selected_int_kind( 9)
    integer, parameter :: int64kind = selected_int_kind(18)

    integer, parameter :: intkind = int64kind

    integer(kind = intkind) :: count,rate

    double precision, save :: ticktime

    if (firstcall) then

       firstcall = .false.

       call system_clock(count, rate)

       ticktime = 1.0d0/dble(rate)
       caftime  = dble(count)*ticktime

       if (this_image() == 1) then
          write(*,*) 'Clock resolution is ', ticktime*1.0e6, ', usecs'
       end if

    else

       call system_clock(count)

       caftime = dble(count)*ticktime

    end if
 
  end function caftime

  logical function cafchecktime(nrep, time, targettime)

    integer :: nrep

    double precision :: time, targettime

    double precision :: tmax, tmin

    tmax = targettime * 4.0/3.0
    tmin = targettime * 2.0/3.0

    cafchecktime = .true.

    if (time > tmax) then

      nrep = max(nrep/2, 1)

    else if (time < tmin) then

      nrep = 2*nrep    
      cafchecktime = .false.

    end if

end function cafchecktime

end module cafclock


