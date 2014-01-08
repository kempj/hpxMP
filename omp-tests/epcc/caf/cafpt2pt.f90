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

module cafpt2pt

contains

  subroutine cafpingpong(image1, image2, cafmodetype, cafsynctype, &
                         maxndata, docheck)

    use cafparams
    use cafclock
    use cafcore

    implicit none

    integer :: cafmodetype, cafsynctype, maxndata
    logical :: docheck

    double precision, allocatable, dimension(:), codimension[:] :: x

    double precision :: time1, time2, time, t0, t1

    integer :: count, nrep, blksize, stride, ndata, nextent, trialnrep
    integer :: irep, i, ierr

    integer :: image1, image2

    double precision :: targettime

    integer, dimension(1) :: neighbours

    logical :: finished, oktime, active, lcheck, gcheck

    gcheck = .true.
    lcheck = .true.

    targettime = p2ptargettime
    trialnrep = 1

    if (cafsynctype /= cafsyncall) then
       active = .false.
    else
       active = .true.
    end if

    neighbours = 0

    if (thisimage == image1) then
       
       active = .true.
       neighbours(1) = image2

    end if

    if (thisimage == image2) then

       active = .true.
       neighbours(1) = image1

    end if

    if (thisimage == 1) then

       write(*,*) "----------------------------------"
       write(*,*) "image1, image2 = ", image1, image2
       write(*,*) "benchmarking: ",    cafmodename(cafmodetype)
       write(*,*) "synchronisation: ", cafsyncname(cafsynctype)
       write(*,*) "maxndata: ", maxndata

       if (docheck) then
          write(*,*) 'verifying data'
       else
          write(*,*) 'NOT verifying data'
       end if
       write(*,*)

       write(*,*) 'count   blksize stride  ndata   nextent nrep    time      latency   bwidth'
       write(*,*)
    end if


    allocate(x(maxndata)[*])

    x(:) = 0

    select case (cafmodetype)

    case(cafmodeput, cafmodesubput, cafmodeget, cafmodesubget, &
         cafmodeallput, cafmodeallget, &
         cafmodesimplesubget, cafmodesimplesubput, cafmodempisend)

       count = 1
       blksize = 1
       stride = 1

    case(cafmodemput, cafmodemsubput, cafmodesubmput, &
         cafmodemget, cafmodemsubget, cafmodesubmget)

       count = 1
       blksize = maxndata
       stride = maxndata

    case(cafmodesmput, cafmodesmget)

       count = 1
       blksize = maxndata/2
       stride = maxndata

    case(cafmodesput, cafmodessubput, cafmodesget, cafmodessubget)

       ! set maximum stride

       stride  = p2pmaxstride

       count = maxndata / stride
       blksize = 1

    case default

       if (thisimage == 1) write(*,*) 'Invalid mode: ', cafmodetype
       stop

    end select

    finished = .false.

    do while (.not. finished)

       ndata   = count*blksize
       nextent = blksize + (count-1)*stride

       oktime = .false.

       if (cafmodetype == cafmodeallput .or. cafmodetype == cafmodeallget) then

          deallocate(x)
          allocate(x(ndata)[*])

       end if

       do while (.not. oktime)

          ! Zero arrays for cleanliness

          x(:) = 0.0

          if (thisimage == image1) then

             ! Force initialisation of source array outside of timing loop 
             ! even if we do not check values within it

             call cafset(x, count, stride, blksize, 1.0d0, .true.)

          end if

          sync all
             

          time1 = caftime()

          do irep = 1, trialnrep

             select case (cafmodetype)

             case (cafmodeput, cafmodesubput, cafmodemput, cafmodemsubput, &
                   cafmodesubmput, &
                   cafmodesput, cafmodessubput, cafmodeallput, &
                   cafmodesimplesubput, cafmodesmput, cafmodempisend)

                if (thisimage == image1) then

                   call cafset(x, count, stride, blksize, &
                               dble(irep), docheck)

                   select case (cafmodetype)

                   case (cafmodeput)

                      x(1:ndata)[image2] = x(1:ndata)

                   case (cafmodeallput)

                      x(:)[image2] = x(:)
 
                  case (cafmodesimplesubput)

                      call cafsimpleput(x, x, ndata, image2)

                   case (cafmodesubput)

                      call cafput(x, x, 1, ndata, image2)

                   case (cafmodemput)

                      do i = 1, count

                         x(1+(i-1)*blksize:i*blksize)[image2] = &
                              x(1+(i-1)*blksize:i*blksize)

                      end do

                   case (cafmodesmput)

                      do i = 1, count

                         x(1+2*(i-1)*blksize:(2*i-1)*blksize)[image2] = &
                         x(1+2*(i-1)*blksize:(2*i-1)*blksize)

                      end do

                   case (cafmodesubmput)

                      call cafmanyput(x, x, 1, count, blksize, image2)

                   case (cafmodemsubput)

                      do i = 1, count

                         call cafput(x, x, 1+(i-1)*blksize, blksize, image2)

                      end do

                   case (cafmodesput)

                      x(1:nextent:stride)[image2] = x(1:nextent:stride)

                   case (cafmodessubput)

                      call cafstridedput(x, x, 1, nextent, stride, image2)

                   case (cafmodempisend)

#ifdef MPI
                      call mpi_send(x, ndata, MPI_DOUBLE_PRECISION, image2-1, &
                                    0, MPI_COMM_WORLD, ierr)
#else
                      write(*,*) 'cafpingpong: ERROR, MPI not enabled but cafmode = ', cafmodetype
                      stop
#endif

                   case  default

                      write(*,*) 'Invalid put mode: ', cafmodetype
                      stop

                   end select

                else if (thisimage == image2) then

                   select case (cafmodetype)

                   case (cafmodempisend)

#ifdef MPI
                      call mpi_recv(x, ndata, MPI_DOUBLE_PRECISION, image1-1, &
                                    0, MPI_COMM_WORLD, status, ierr)
#else
                      write(*,*) 'cafpingpong: ERROR, MPI not enabled but cafmode = ', cafmodetype
                      stop
#endif

                   end select

                end if

                call cafdosync(cafsynctype, active, neighbours)

                if (thisimage == image2) then

                   lcheck = cafcheck(x, count, stride, blksize, &
                                     dble(irep), docheck)

                   call cafset(x, count, stride, blksize, &
                               dble(-irep), docheck)

                   select case (cafmodetype)

                   case (cafmodeput)

                      x(1:ndata)[image1] = x(1:ndata)

                   case (cafmodeallput)

                      x(:)[image1] = x(:)

                   case (cafmodesimplesubput)

                      call cafsimpleput(x, x, ndata, image1)

                   case (cafmodesubput)

                      call cafput(x, x, 1, ndata, image1)

                   case (cafmodemput)

                      do i = 1, count

                         x(1+(i-1)*blksize:i*blksize)[image1] = &
                              x(1+(i-1)*blksize:i*blksize)

                      end do

                   case (cafmodesmput)

                      do i = 1, count

                         x(1+2*(i-1)*blksize:(2*i-1)*blksize)[image1] = &
                         x(1+2*(i-1)*blksize:(2*i-1)*blksize)

                      end do

                   case (cafmodesubmput)

                      call cafmanyput(x, x, 1, count, blksize, image1)

                   case (cafmodemsubput)

                      do i = 1, count

                         call cafput(x, x, 1+(i-1)*blksize, blksize, image1)

                      end do

                   case (cafmodesput)

                      x(1:nextent:stride)[image1] = x(1:nextent:stride)

                   case (cafmodessubput)

                      call cafstridedput(x, x, 1, nextent, stride, image1)

                   case (cafmodempisend)

#ifdef MPI
                      call mpi_send(x, ndata, MPI_DOUBLE_PRECISION, image1-1, &
                                    0, MPI_COMM_WORLD, ierr)
#else
                      write(*,*) 'cafpingpong: ERROR, MPI not enabled but cafmode = ', cafmodetype
                      stop
#endif

                   case  default

                      write(*,*) 'Invalid put mode: ', cafmodetype
                      stop


                   end select

                else if (thisimage == image1) then

                   select case (cafmodetype)

                   case (cafmodempisend)

#ifdef MPI
                      call mpi_recv(x, ndata, MPI_DOUBLE_PRECISION, image2-1, &
                                    0, MPI_COMM_WORLD, status, ierr)
#else
                      write(*,*) 'cafpingpong: ERROR, MPI not enabled but cafmode = ', cafmodetype
                      stop
#endif

                   end select

                end if

                if (docheck) then
                  gcheck = gand(lcheck)
                end if

                if (gcheck .eqv. .false.) then

                   if (thisimage == 1) then
                      write(*,*) 'ERROR: put ping failed to validate'
                      write(*,*) 'ERROR: cnt,blk,str,dat,ext,irp,nrp = ', &
                           count, blksize, stride, ndata, nextent, irep, nrep
                   end if

                   exit

                end if
                      
                call cafdosync(cafsynctype, active, neighbours)

                if (thisimage == image1) then
                   lcheck = cafcheck(x, count, stride, blksize, &
                                     dble(-irep), docheck)
                end if

                if (docheck) then
                  gcheck = gand(lcheck)
                end if

                if (gcheck .eqv. .false.) then

                   if (thisimage == 1) then
                      write(*,*) 'ERROR: put pong failed to validate'
                      write(*,*) 'ERROR: cnt, blk, str, dat, ext, rep = ', &
                                  count, blksize, stride, ndata, nextent, nrep
                   end if

                   exit

                end if

                ! Get cases

             case (cafmodeget, cafmodesubget, cafmodemget, cafmodemsubget, &
                   cafmodesubmget, &
                   cafmodesget, cafmodessubget, cafmodeallget, &
                   cafmodesimplesubget, cafmodesmget)

                if (thisimage == image2) then

                   select case (cafmodetype)

                   case (cafmodeget)

                      x(1:ndata) = x(1:ndata)[image1]

                   case (cafmodeallget)

                      x(:) = x(:)[image1]

                   case (cafmodesimplesubget)

                      call cafsimpleget(x, x, ndata, image1)

                   case (cafmodesubget)

                      call cafget(x, x, 1, ndata, image1)

                   case (cafmodemget)

                      do i = 1, count

                         x(1+(i-1)*blksize:i*blksize) =        &
                              x(1+(i-1)*blksize:i*blksize)[image1]

                      end do

                   case (cafmodesget)

                      x(1:nextent:stride) = x(1:nextent:stride)[image1]

                   case (cafmodesmget)

                      do i = 1, count

                         x(1+2*(i-1)*blksize:(2*i-1)*blksize) = &
                         x(1+2*(i-1)*blksize:(2*i-1)*blksize)[image1]

                      end do

                   case (cafmodesubmget)

                      call cafmanyget(x, x, 1, count, blksize, image1)

                   case (cafmodemsubget)

                      do i = 1, count

                         call cafget(x, x, 1+(i-1)*blksize, blksize, image1)

                      end do

                   case (cafmodessubget)

                      call cafstridedget(x, x, 1, nextent, stride, image1)

                   case  default

                      write(*,*) 'Invalid get mode: ', cafmodetype
                      stop

                   end select

                   lcheck = cafcheck(x, count, stride, blksize, &
                                    dble(irep), docheck)

                   call cafset(x, count, stride, blksize, &
                               dble(-irep), docheck)

                end if

                if (docheck) then
                  gcheck = gand(lcheck)
                end if

                if (gcheck .eqv. .false.) then

                   if (thisimage == 1) then
                      write(*,*) 'ERROR: get ping failed to validate'
                      write(*,*) 'ERROR: cnt,blk,str,dat,ext,irp,nrp = ', &
                           count, blksize, stride, ndata, nextent, irep, nrep
                   end if

                   exit

                end if

                call cafdosync(cafsynctype, active, neighbours)
                
                if (thisimage == image1) then

                   select case (cafmodetype)

                   case (cafmodeget)

                      x(1:ndata) = x(1:ndata)[image2]

                   case (cafmodeallget)

                      x(:) = x(:)[image2]

                   case (cafmodesimplesubget)

                      call cafsimpleget(x, x, ndata, image2)

                   case (cafmodesubget)

                      call cafget(x, x, 1, ndata, image2)

                   case (cafmodemget)

                      do i = 1, count

                         x(1+(i-1)*blksize:i*blksize)         =  &
                              x(1+(i-1)*blksize:i*blksize)[image2]

                      end do

                   case (cafmodesubmget)

                      call cafmanyget(x, x, 1, count, blksize, image2)

                   case (cafmodemsubget)

                      do i = 1, count

                         call cafget(x, x, 1+(i-1)*blksize, blksize, image2)

                      end do

                   case (cafmodesget)

                      x(1:nextent:stride) = x(1:nextent:stride)[image2]

                   case (cafmodesmget)

                      do i = 1, count

                         x(1+2*(i-1)*blksize:(2*i-1)*blksize) = &
                         x(1+2*(i-1)*blksize:(2*i-1)*blksize)[image2]

                      end do

                   case (cafmodessubget)

                      call cafstridedget(x, x, 1, nextent, stride, image2)

                   case  default

                      write(*,*) 'Invalid get mode: ', cafmodetype
                      stop

                   end select

                   lcheck = cafcheck(x, count, stride, blksize, &
                                     dble(-irep), docheck)

                   call cafset(x, count, stride, blksize, &
                               dble(irep+1), docheck)

                end if

                if (docheck) then
                  gcheck = gand(lcheck)
                end if

                if (gcheck .eqv. .false.) then

                   if (thisimage == 1) then
                      write(*,*) 'ERROR: get pong failed to validate'
                      write(*,*) 'ERROR: cnt,blk,str,dat,ext,irp,nrp = ', &
                           count, blksize, stride, ndata, nextent, irep, nrep
                   end if

                   exit

                end if

                call cafdosync(cafsynctype, active, neighbours)

             end select

          end do

          sync all

          time2 = caftime()
          time = time2 - time1

! Broadcast time from image 1

          call dbcast(time, 1)

          nrep = trialnrep
          oktime = cafchecktime(trialnrep, time, targettime)

          if (gcheck .eqv. .false.) then
             exit
          end if

       end do

       if (thisimage == 1) then

          if (gcheck) then

             write(*,fmt='(5(i7, 1x), i6, 1x, 2(g10.3, 1x), g11.4)') &
                  count, blksize, stride, ndata, nextent, nrep, &
                  time, time/dble(2*nrep), &
                  (2.0*dble(nrep)*dble(ndata)*dble(dblesize)) /(dble(1024*1024)*time)
          else

             write(*,*)
             write(*,*) 'Verification failed: exiting this test'
             write(*,*)

          end if

       end if

       if (gcheck .eqv. .false.) then

          finished = .true.
          exit

       end if
          
       select case (cafmodetype)

       case(cafmodeput, cafmodesubput, cafmodeget, cafmodesubget, &
            cafmodeallput, cafmodeallget, &
            cafmodesimplesubput, cafmodesimplesubget, cafmodempisend)

          blksize = 2*blksize
          stride  = 2*stride
          if (blksize > maxndata) finished = .true.

       case(cafmodemput, cafmodemsubput, cafmodesubmput, &
            cafmodemget, cafmodemsubget, cafmodesubmget)

          count = 2*count
          blksize = blksize/2
          stride = stride/2
          if (count > maxndata) finished = .true.

       case(cafmodesmput, cafmodesmget)

          count = 2*count
          blksize = blksize/2
          stride = stride/2

          if (stride < 2) finished = .true.

       case(cafmodesput, cafmodessubput, cafmodesget, cafmodessubget)

          stride = stride / 2
          if (stride < 1) finished = .true.

       case default

          if (thisimage == 1) write(*,*) 'Invalid mode: ', cafmodetype
          stop

       end select

    end do

    deallocate(x)

    if (thisimage == 1) then

       write(*,*)

       if (docheck) then

          if (gcheck) then

             write(*,*)  'All results validated'
             write(*,*)

          else

             write(*,*)
             write(*,*) 'ERROR: validation failed'
             write(*,*)

          end if

       end if

    end if

  end subroutine cafpingpong


  subroutine cafoverlap(image1, image2, cafsynctype, maxndata)

    use cafcore
    use cafclock

    implicit none

    integer :: maxndata, ndata

    double precision, allocatable, dimension(:), codimension[:] :: x
    double precision :: time1, time2, puttime, dltime

    integer :: irep, nputrep, ndelayrep, iloop
    integer :: image1, image2, cafsynctype

    double precision :: puttargettime, dltargettime

    logical, save :: oktime[*]
    integer, save :: trialnrep[*]

    integer, dimension(1) :: neighbours
    logical :: active

! !DIR$ INLINENEVER delay

    if (thisimage == 1) then

       write(*,*) "image1, image2 = ", image1, image2
       write(*,*) "benchmarking: overlap"
       write(*,*) "synchronisation: ", cafsyncname(cafsynctype)
       write(*,*) "maxndata: ", maxndata

    end if

    allocate(x(maxndata)[*])

    if (cafsynctype /= cafsyncall) then
       active = .false.
    else
       active = .true.
    end if

    neighbours = 0

    if (thisimage == image1) then
       
       active = .true.
       neighbours(1) = image2

    end if

    if (thisimage == image2) then

       active = .true.
       neighbours(1) = image1

    end if

    x(:) = dble(thisimage)

    puttargettime = 0.5
    puttime = 0.0

    trialnrep = 1
    oktime = .false.

    do while (.not. oktime)

       sync all

       if (thisimage == image1) then

          time1 = caftime()

          do irep = 1, trialnrep

             x(:)[image2] = x(:)

             call cafdosync(cafsynctype, active, neighbours)

          end do
             
          time2 = caftime()
        
          puttime = time2 - time1

          oktime = cafchecktime(trialnrep, puttime, puttargettime)

          sync all

       else

          do irep = 1, trialnrep
 
             call cafdosync(cafsynctype, active, neighbours)

! Do something with data on the target

             if (thisimage == image2) then
             
               x(:) = x(:) + 1.0
               if (x(1) < 0.0) write(*,*) 'Tum te tum'

            end if

         end do

! Make sure all images get flag from image 1

         sync all

         oktime    = oktime[1]
         trialnrep = trialnrep[1]

      end if

   end do
       

   nputrep = trialnrep
   write(*,*) 'image: ', thisimage, ', nputrep, puttime = ', nputrep, puttime

! Now we need to match this with a delay

   dltargettime = puttargettime / dble(nputrep)

   trialnrep = 1
   oktime = .false.

   do while (.not. oktime)

      time1 = caftime()

      call delay(trialnrep)

      time2 = caftime()
      dltime = time2 - time1
       
      oktime = cafchecktime(trialnrep, dltime, dltargettime)

   end do

   write(*,*) 'image, trialnrep, dltime, nputrep*dltime = ', &
               thisimage, trialnrep, dltime, dble(nputrep)*dltime

   trialnrep = dble(trialnrep)*puttime/(dble(nputrep)*dltime)

   sync all
   ndelayrep = trialnrep[1]
   sync all

! Now time for real
       
   time1 = caftime()

   do irep = 1, nputrep
      call delay(ndelayrep)
   end do

   time2 = caftime()
   dltime = time2 - time1

   write(*,*) 'image, ndelayrep, nputrep*dltime = ', &
               thisimage, ndelayrep, dltime

! OK, now we time overlapped and non-overlapped stuff
! Probably best to have the targets doing work as well to better
! simulate a real case

    do iloop = 1, 4

       if (thisimage == 1) then

          write(*,*) 'WITH magic directive'

          if ((iloop-1)/2 .eq. 0) write(*,*) 'Put'
          if ((iloop-1)/2 .eq. 1) write(*,*) 'Get'
          if (mod(iloop-1,2) .eq. 0) write(*,*) 'No overlap'
          if (mod(iloop-1,2) .eq. 1) write(*,*) 'With overlap'

          write(*,*)
          
       end if

       sync all

       if (thisimage == image1) then

          time1 = caftime()

          do irep = 1, nputrep

             if ((iloop-1)/2 .eq. 0) then
! Directive to help overlaping
!DIR$ PGAS DEFER_SYNC
                x(:)[image2] = x(:)
             end if

             if ((iloop-1)/2 .eq. 1) then
! Directive to help overlaping
!DIR$ PGAS DEFER_SYNC
                x(:) = x(:)[image2]
             end if

             if (mod(iloop-1,2) .eq. 0) then
                call cafdosync(cafsynctype, active, neighbours)
             end if

             call delay(ndelayrep)

             if (mod(iloop-1,2) .eq. 1) then
                call cafdosync(cafsynctype, active, neighbours)
             end if

             if ((iloop-1)/2 .eq. 1) then

! Get case - do something with data on the target

               x(:) = x(:) + 1.0

               if (x(1) < 0.0) write(*,*) 'Tum te tum'

            end if
 
         end do
            
         time2 = caftime()
         puttime = time2 - time1

         write(*,*) 'iloop, image, time = ', iloop, thisimage, puttime

      else

         do irep = 1, nputrep

             call delay(ndelayrep)

             call cafdosync(cafsynctype, active, neighbours)

! Do something with data on the target for put case

             if ((iloop-1)/2 .eq. 0) then

                if (thisimage == image2) then

                   x(1) = x(1) + 1.0

                   if (x(1) < 0.0) write(*,*) 'Tum te tum'

                end if

             end if
          end do

      end if

   end do
       
   deallocate(x)
  
 end subroutine cafoverlap

end module cafpt2pt
