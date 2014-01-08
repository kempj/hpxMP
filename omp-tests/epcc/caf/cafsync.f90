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

module cafsync

  use iso_fortran_env
  use cafcore
  use cafclock

  implicit none

  double precision :: conf95 = 1.96

contains

subroutine cafsyncbench(outerreps, innercount, dl, cafsynctype, numneigh)

  integer :: outerreps, innercount, innerreps, dl, cafsynctype, numneigh

  integer, allocatable, dimension(:) :: neighbours

  logical :: active = .true.

  double precision, dimension(0:outerreps) :: time

  double precision :: tstart, tstop
  double precision :: meantime, totaltime, sd, reftime, refsd

  integer :: upimage, dnimage, imageshift  

  integer :: i, j, k, nout

  type(lock_type), save :: caflock[*]

! !DIR$ INLINENEVER delay

  if (thisimage == 1) then

    write(*,*) 'cafsync: Synchronisation type: ', cafsyncname(cafsynctype)
    write(*,*) 'cafsync: outerreps, innercount, delay = ', outerreps, innercount, dl

  end if

  select case (cafsynctype)

  case (cafsyncring, cafsyncrand, cafsync3d, cafsyncpair)

     if (numneigh .ge. numimages) then
        write(*,*) 'cafsync: numneigh too large!'
        stop
     else if (thisimage == 1) then
        write(*,*) 'cafsync: Number of neighbours is ', numneigh
     end if

     allocate(neighbours(numneigh))

     call getneighs(thisimage, numimages, neighbours, cafsynctype)
        
  case (cafsyncall, cafsyncmpi, cafsynccrit, cafsynclock)

  case default

     write(*,*) 'cafsync: Illegal cafsynctype: ', cafsynctype
     stop

  end select

  innerreps = innercount

  if (cafsynctype == cafsynclock .or. cafsynctype == cafsynccrit) then

! cope with the fact tha we scale innerreps by numimages for serialised
! synchronisation. 

    innerreps = max(2, innercount/numimages)
    innerreps = innerreps * numimages

    if (innerreps /= innercount) then
       if (thisimage == 1) then
          write(*,*) 'cafsync: rescaling reference innercount to ', innerreps
       end if
    end if

  end if

  sync all

! Reference time - always discard iteration 0

  do k = 0, outerreps

    tstart = caftime()

    do j = 1, innerreps
      call delay(dl)
    end do

    tstop = caftime()

    time(k) = (tstop - tstart) / dble (innerreps)
    
  end do

  totaltime = sum(time(1:outerreps))*dble(innerreps)


  call stats (time, outerreps, meantime, sd, nout)
  
  reftime = meantime 
  refsd = sd 

  if (thisimage == 1) then
     write(*,*) 'cafsync: reference time is ', reftime*1.0e6, &
                ' +/- ', refsd*1.0e6, 'usec'
!     write(*,*) 'cafsync: median time    is', mediantime*1.0e6, 'usecs'
     write(*,*) 'cafsync: number of outliers was ', nout
  end if

  sync all  

! Now do actual sync - always discard iteration 0

  do k = 0, outerreps

    sync all

    tstart = caftime()

    select case (cafsynctype)

    case (cafsyncall, cafsyncmpi, &
          cafsyncring, cafsyncrand, cafsync3d, cafsyncpair)

       innerreps = innercount

       do j = 1, innerreps

          call delay(dl)
          call cafdosync(cafsynctype, active, neighbours)

       end do

    case (cafsynccrit)

       innerreps = max(2, innercount/numimages)

       do j = 1, innerreps

          critical

          call delay(dl)

          end critical

       end do

    case (cafsynclock)

       innerreps = max(2, innercount/numimages)

       do j = 1, innerreps

          lock (caflock[1])

          call delay(dl)

          unlock (caflock[1])

       end do

    case default

       write(*,*) 'cafsync: Illegal cafsynctype: ', cafsynctype
       stop

    end select

    sync all

    tstop = caftime()

    time(k) = (tstop - tstart) / dble (innercount)
    
  end do

  totaltime = sum(time(1:outerreps))*dble(innercount)

  call stats (time, outerreps, meantime, sd, nout)

  if (thisimage == 1) then
     write(*,*) 'cafsync: elapsed   is ', totaltime, ' seconds'
     write(*,*) 'cafsync: loop time is ', meantime*1.0e6, ' +/- ', sd*1.0e6, 'usecs'
!     write(*,*) 'cafsync: median    is ', mediantime*1.0e6, 'usecs'
     write(*,*) 'cafsync: number of outliers was ', nout
     write(*,*) 'cafsync: sync time is ', (meantime - reftime)*1.0e6, ' +/- ', conf95*(sd+refsd)*1.0e6, ' usecs'
     write(*,*)

!     do k = 0, outerreps
!        write(*,*) k, time(k)*1.0e6
!     end do

     write(*,*)

  end if

  return

end subroutine cafsyncbench

subroutine stats (time, outerreps, meantime, sd, nr)

  implicit none

  integer :: outerreps

  double precision, dimension(0:outerreps) :: time
!  double precision, allocatable, dimension(:) :: tmptime

  double precision :: meantime, totaltime
  double precision :: sumsq, mintime, maxtime, sd
  double precision :: cutoff
  integer :: nr, i

!  allocate(tmptime(outerreps))

  mintime = time(1)
  maxtime = time(1)
  totaltime = 0.0

  do i = 2, outerreps

     mintime = min(mintime,time(i))
     maxtime = max(maxtime,time(i))
     totaltime = totaltime + time(i)

  end do

  meantime  = totaltime / dble(outerreps)

  sumsq = 0.

  do i = 1, outerreps
     sumsq = sumsq + (time(i)-meantime)**2 
  end do

  sd = sqrt(sumsq/dble(outerreps-1))

! COUNT OUTLIERS 

  cutoff = 3.0 * sd 
  nr = 0 

  do i = 1, outerreps
     if (abs(time(i)-meantime) > cutoff) nr = nr + 1
  end do

!  tmptime(1:outerreps) = time(1:outerreps)
!
!  call qsort(tmptime)
!
!  if (mod(outerreps,2) == 1) then
!     mediantime = tmptime((outerreps+1)/2)
!  else
!     mediantime = (tmptime(outerreps/2) + tmptime(outerreps/2 + 1))/2.0d0
!  end if

!  if (thisimage == 1) write (*,*)
!  if (thisimage == 1) write(*,'(a11,8x,a7,6x,a3,10x,a3,11x,a5,6x,a8)')&
!       'Sample_size','Average','Min','Max','S.D.','Outliers'
!  if (thisimage == 1) write (*,1002) outerreps, meantime, mintime,maxtime, sd, nr 
!  if (thisimage == 1) write (*,*) 'mediantime = ', mediantime
!  if (thisimage == 1) write (*,*)
!1002 format(4x,i7,4x,4f13.5,4x,i7)
!
!  deallocate(tmptime)

  return

end subroutine stats

end module cafsync
