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

module cafpt2ptdrivermod

  use cafparams
  use cafcore
  use cafpt2pt

  implicit none

contains

subroutine cafpt2ptdriver

  logical :: docheck = p2pcheck

  logical :: dosingle = p2psingle
  logical :: domulti  = p2pmulti
  logical :: docross  = p2pcross


  integer :: image1, image2, tmpimage, nmax
  integer :: iloop, sloop
  integer :: cafsynctype, cafmodetype

  nmax = p2pnmax

  image1 = 0
  image2 = 0

  do iloop = 1, 3

     if (iloop == 1 .and. dosingle) then

        image1 = 1
        image2 = numimages

        if (thisimage == 1) then
           write(*,*) '--------------------'
           write(*,*) ' Single ping-pong'
           write(*,*) '--------------------'
           write(*,*)
        end if

     else if (iloop == 2 .and. domulti) then

        if (numimages > 2 .and. mod(numimages,2) == 0) then

           if (thisimage <= numimages/2) then
              image1 = thisimage
              image2 = thisimage + numimages/2
           else
              image2 = thisimage
              image1 = thisimage - numimages/2
           end if

           if (thisimage == 1) then
              write(*,*) '--------------------'
              write(*,*) ' Multiple ping-pong'
              write(*,*) '--------------------'
              write(*,*)
           end if

        else

           if (thisimage == 1) then
              write(*,*) '-----------------------------------------------------'
              write(*,*) ' Cannot do multiple pingpong with numimages = ', numimages
              write(*,*) '-----------------------------------------------------'
           end if
           
           domulti = .false.

        end if

     else if (iloop == 3 .and. docross) then

        if (mod(numimages, 4) == 0) then

           ! First pair them up as before

           if (thisimage <= numimages/2) then
              image1 = thisimage
              image2 = thisimage + numimages/2
           else
              image2 = thisimage
              image1 = thisimage - numimages/2
           end if

           ! Assuming an even number of pairs we can swap all the even ones

           if (mod(thisimage,2) == 0) then

              tmpimage = image1
              image1 = image2
              image2 = tmpimage

           end if

           if (thisimage == 1) then
              write(*,*) '-----------------------'
              write(*,*) '  Crossing ping-pong'
              write(*,*) '-----------------------'
           end if
           
        else

           if (thisimage == 1) then
              write(*,*) '-----------------------------------------------------'
              write(*,*) ' Cannot do crossing pingpong with numimages = ', numimages
              write(*,*) '-----------------------------------------------------'
           end if
           
           docross = .false.

        end if

     end if

     if ( (iloop == 1 .and. dosingle)  .or. &
          (iloop == 2 .and. domulti )  .or. &
          (iloop == 3 .and. docross )) then

        do sloop = 1, 2

           if (sloop == 1) cafsynctype = cafsyncall
           if (sloop == 2) cafsynctype = cafsyncpt2pt

           do cafmodetype = 1, maxcafmode

              if (cafmodetype == cafmodempisend) cafsynctype = cafsyncnull

              call cafpingpong(image1, image2, cafmodetype, cafsynctype, &
                   nmax, docheck)

           end do

        end do

     end if

  end do
  

end subroutine cafpt2ptdriver

end module cafpt2ptdrivermod
