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

module cafhalo

  use cafparams
  use cafcore
  use cafclock

  implicit none

contains

subroutine cafhaloswap(dimx, dimy, dimz, codimx, codimy, codimz, &
                         cafsynctype, cafhalotype, docheck)

  integer :: dimx, dimy, dimz, codimx, codimy, codimz

  double precision, allocatable, dimension(:,:,:), codimension[:,:,:] :: x

  integer :: coordx, coordy, coordz
  integer :: upcoordx, upcoordy, upcoordz
  integer :: dncoordx, dncoordy, dncoordz
  integer :: upimagex, upimagey, upimagez
  integer :: dnimagex, dnimagey, dnimagez

  double precision :: time1, time2, time

  integer :: i, ndata, nextent, irep, nrep, trialnrep
  integer :: cafsynctype, cafhalotype

  double precision :: targettime

  integer, parameter :: ndims = 3

  logical :: finished, oktime, docheck, active, lcheck, gcheck
  logical :: hcheck(2*ndims)

  double precision :: myhaloval

  integer, dimension(2*ndims) :: neighbours
  double precision, dimension(2*ndims) :: neighhaloval

  integer, dimension(ndims) :: imagecoord

  if (codimx*codimy*codimz /= numimages) then

     if (thisimage == 1) then
        write(*,*) 'Error: invalid codims for numimages = ', numimages
     end if

     stop

  end if

  allocate(x(0:dimx+1, 0:dimy+1, 0:dimz+1)[codimx, codimy, *])

  targettime = halotargettime

  trialnrep = 1

  ndata = 2*((dimy*dimz) + (dimx*dimz) + (dimx*dimy))

  imagecoord(1:ndims) = this_image(x)

  coordx = imagecoord(1)
  coordy = imagecoord(2)
  coordz = imagecoord(3)

  upcoordx = mod(coordx, codimx) + 1
  upcoordy = mod(coordy, codimy) + 1
  upcoordz = mod(coordz, codimz) + 1

  dncoordx = mod(coordx+codimx-2, codimx) + 1
  dncoordy = mod(coordy+codimy-2, codimy) + 1
  dncoordz = mod(coordz+codimz-2, codimz) + 1

  neighbours(1) = image_index(x, [upcoordx, coordy, coordz])
  neighbours(2) = image_index(x, [coordx, upcoordy, coordz])
  neighbours(3) = image_index(x, [coordx, coordy, upcoordz])
  neighbours(4) = image_index(x, [dncoordx, coordy, coordz])
  neighbours(5) = image_index(x, [coordx, dncoordy, coordz])
  neighbours(6) = image_index(x, [coordx, coordy, dncoordz])


  active = .true.

  if (thisimage == 1) then

     write(*,*) "cafhaloswap: synchronisation: ", cafsyncname(cafsynctype)
     write(*,*) "cafhaloswap: transfer mode:   ", cafhaloname(cafhalotype)

     if (docheck) then
        write(*,*) 'cafhaloswap: verifying data'
     else
        write(*,*) 'cafhaloswap: NOT verifying data'
     end if

  end if

  oktime = .false.

  ! Because boundary data is copied to multiple halos (eg corner data
  ! is replicated even without diagonal neighbours) we have to set all
  ! the halos to the same value to allow for (simple) verification

  ! Multiply by a big number so we can make these (mostly) unique per
  ! iteration by adding on irep

  myhaloval = thisimage*1000

  neighhaloval(1) = neighbours(1)*1000
  neighhaloval(2) = neighbours(2)*1000
  neighhaloval(3) = neighbours(3)*1000
  neighhaloval(4) = neighbours(4)*1000
  neighhaloval(5) = neighbours(5)*1000
  neighhaloval(6) = neighbours(6)*1000

  do while (.not. oktime)

     lcheck = .true.
     gcheck = .true.
     hcheck(:) = .true.

     x(:,:,:) = 0.0

     x(dimx  , 1:dimy, 1:dimz) = dble(myhaloval)
     x(1:dimx, dimy,   1:dimz) = dble(myhaloval)
     x(1:dimx, 1:dimy, dimz  ) = dble(myhaloval)

     x(1,      1:dimy, 1:dimz) = dble(myhaloval)
     x(1:dimx, 1,      1:dimz) = dble(myhaloval)
     x(1:dimx, 1:dimy, 1     ) = dble(myhaloval)

     sync all

     time1 = caftime()

     do irep = 1, trialnrep

        ! Now set boundaries to iteration-dependent value if we
        ! are verifyiing data

        call cafset2d(x(dimx  , 1:dimy, 1:dimz), 1, dimy*dimz, dimy*dimz, &
                      dble(myhaloval+irep), docheck)

        call cafset2d(x(1:dimx, dimy,   1:dimz), 1, dimx*dimz, dimx*dimz, &
                      dble(myhaloval+irep), docheck)

        call cafset2d(x(1:dimx, 1:dimy, dimz  ), 1, dimx*dimy, dimx*dimy, &
                      dble(myhaloval+irep), docheck)

        call cafset2d(x(1,      1:dimy, 1:dimz), 1, dimy*dimz, dimy*dimz, &
                      dble(myhaloval+irep), docheck)

        call cafset2d(x(1:dimx, 1,      1:dimz), 1, dimx*dimz, dimx*dimz, &
                      dble(myhaloval+irep), docheck)

        call cafset2d(x(1:dimx, 1:dimy, 1     ), 1, dimx*dimy, dimx*dimy, &
                      dble(myhaloval+irep), docheck)

        ! Synchronise so we know we can change halo values on other images
        ! ie wait until halo data has been processed (put) or ensure boundary
        ! data is ready on other images (get)

        call cafdosync(cafsynctype, active, neighbours)

        select case (cafhalotype)

        case(cafmodehalopp)

! !DIR$ PGAS DEFER_SYNC
           x(0,      1:dimy, 1:dimz)[upcoordx, coordy, coordz] = &
                x(dimx,   1:dimy, 1:dimz)

! !DIR$ PGAS DEFER_SYNC
           x(1:dimx, 0,      1:dimz)[coordx, upcoordy, coordz] = &
                x(1:dimx, dimy,   1:dimz)

! !DIR$ PGAS DEFER_SYNC
           x(1:dimz, 1:dimy, 0     )[coordx, coordy, upcoordz] = &
                x(1:dimx, 1:dimy, dimz  )


           ! Send bottom boundaries downwards

! !DIR$ PGAS DEFER_SYNC
           x(dimx+1, 1:dimy, 1:dimz)[dncoordx, coordy, coordz] = &
                x(1,      1:dimy, 1:dimz)

! !DIR$ PGAS DEFER_SYNC
           x(1:dimx, dimy+1, 1:dimz)[coordx, dncoordy, coordz] = &
                x(1:dimx, 1,      1:dimz)

! !DIR$ PGAS DEFER_SYNC
           x(1:dimx, 1:dimy, dimz+1)[coordx, coordy, dncoordz] = &
                x(1:dimx, 1:dimy, 1)

        case(cafmodehalogg)

           ! Get bottom halo from downward neighbours

! !DIR$ PGAS DEFER_SYNC
           x(0,      1:dimy, 1:dimz) = &
                x(dimx,   1:dimy, 1:dimz)[dncoordx, coordy, coordz]

! !DIR$ PGAS DEFER_SYNC
           x(1:dimx, 0,      1:dimz) = &
                x(1:dimx, dimy,   1:dimz)[coordx, dncoordy, coordz]

! !DIR$ PGAS DEFER_SYNC
           x(1:dimz, 1:dimy, 0     ) = &
                x(1:dimx, 1:dimy, dimz  )[coordx, coordy, dncoordz]

           ! Get top halo from upward neighbour

! !DIR$ PGAS DEFER_SYNC
           x(dimx+1, 1:dimy, 1:dimz) = &
                x(1,      1:dimy, 1:dimz)[upcoordx, coordy, coordz]

! !DIR$ PGAS DEFER_SYNC
           x(1:dimx, dimy+1, 1:dimz) = &
                x(1:dimx, 1,      1:dimz)[coordx, upcoordy, coordz]

! !DIR$ PGAS DEFER_SYNC
           x(1:dimx, 1:dimy, dimz+1) = &
                x(1:dimx, 1:dimy, 1     )[coordx, coordy, upcoordz]

        case default

           write(*,*) 'cafhaloswap: invalid mode ', cafhaloname(cafhalotype)
           stop

        end select

        ! Now wait for all halo data to come in before proceeding (put)
        ! or wait for other images to read boundary data before altering (get)

        call cafdosync(cafsynctype, active, neighbours)


        hcheck(1) = cafcheck2d(x(dimx+1, 1:dimy, 1:dimz), 1, dimy*dimz, &
                               dimy*dimz, dble(neighhaloval(1)+irep), docheck)

        hcheck(2) = cafcheck2d(x(1:dimx, dimy+1, 1:dimz), 1, dimx*dimz, &
                               dimx*dimz, dble(neighhaloval(2)+irep), docheck)

        hcheck(3) = cafcheck2d(x(1:dimx, 1:dimy, dimz+1), 1, dimx*dimy, &
                               dimx*dimy, dble(neighhaloval(3)+irep), docheck)

        hcheck(4) = cafcheck2d(x(0, 1:dimy, 1:dimz), 1, dimy*dimz, &
                               dimy*dimz, dble(neighhaloval(4)+irep), docheck)

        hcheck(5) = cafcheck2d(x(1:dimx, 0, 1:dimz), 1, dimx*dimz,  &
                               dimx*dimz, dble(neighhaloval(5)+irep), docheck)

        hcheck(6) = cafcheck2d(x(1:dimx, 1:dimy, 0), 1, dimx*dimy, &
                               dimx*dimy, dble(neighhaloval(6)+irep), docheck)

        do i = 1, 2*ndims
           lcheck = lcheck .and. hcheck(i)
        end do

        if (docheck) then

           gcheck = gand(lcheck)

           if (gcheck .eqv. .false.) then
          
              exit
              
           end if

        end if

     end do

     sync all

     time2 = caftime()
     time = time2 - time1

     call dbcast(time, 1)

     nrep = trialnrep
     oktime = cafchecktime(trialnrep, time, targettime)

     if (gcheck .eqv. .false.) then
          
        exit

     end if

  end do

  if (thisimage == 1) then

     write(*,*) 'cafhaloswap: ndata, nrep, time = ', &
          ndata, nrep, time

     write(*,*) 'cafhaloswap: latency = ', time/dble(nrep), ' secs'
     write(*,*) 'cafhaloswap: bwidth  = ', &
          (dble(nrep)*dble(ndata)*dble(dblesize)) / &
          (dble(1024*1024)*time), ' MB/s'
     write(*,*)

     if (docheck) then

        if (gcheck) then

           write(*,*) 'cafhaloswap: All results validated'
           write(*,*)

        else

           write(*,*) 'cafhaloswap: ERROR, test FAILED to verify'
           write(*,*)
        
        end if

     end if

  end if

  deallocate(x)

end subroutine cafhaloswap

end module cafhalo
