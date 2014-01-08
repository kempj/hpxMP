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

program cafbench

  use cafcore

  use cafpt2ptdrivermod
  use cafsyncdrivermod
  use cafhalodrivermod

  implicit none

  logical :: dopt2pt, dosync, dohalo

  dopt2pt = p2pbench
  dosync  = syncbench
  dohalo  = halobench

  call cafinit()

  if (thisimage == 1) then

     write(*,*) '------------------'
     write(*,*) 'CAF Benchmark'
     write(*,*)
     write(*,*) 'numimages = ', numimages
     write(*,*) '------------------'
     write(*,*)
     
  end if


  if (dopt2pt) then
 
     if (thisimage == 1) then
        
        write(*,*) '--------------'
        write(*,*) 'Point-to-point'
        write(*,*) '--------------'
        write(*,*)

     end if

     call cafpt2ptdriver()
     
  end if


  if (dosync) then

     if (thisimage == 1) then

        write(*,*) '---------------'
        write(*,*) 'Synchronisation'
        write(*,*) '---------------'
        write(*,*)

     end if

     call cafsyncdriver()

  end if

  if (dohalo) then

     if (thisimage == 1) then

        write(*,*) '-------------'
        write(*,*) '3D Halo Swaps'
        write(*,*) '-------------'
        write(*,*)

     end if

     call cafhalodriver()

  end if

  if (thisimage == 1) then

     write(*,*) '------------------'
     write(*,*) 'Benchmark finished'
     write(*,*) '------------------'
     
  end if

  call cafend()

end program cafbench
