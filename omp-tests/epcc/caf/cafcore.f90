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

module cafcore

#ifdef MPI
  use mpi
#endif
  use cafclock

  implicit none

  integer, parameter :: maxchar    = 32

#ifdef MPI
  integer, dimension(MPI_STATUS_SIZE) :: status

  integer, parameter :: maxcafmode = 21
#else
  integer, parameter :: maxcafmode = 20
#endif

  integer, parameter :: cafmodeput          =  1
  integer, parameter :: cafmodesubput       =  2
  integer, parameter :: cafmodemput         =  3
  integer, parameter :: cafmodesubmput      =  4
  integer, parameter :: cafmodemsubput      =  5
  integer, parameter :: cafmodesput         =  6
  integer, parameter :: cafmodessubput      =  7
  integer, parameter :: cafmodeallput       =  8
  integer, parameter :: cafmodesimplesubput =  9
  integer, parameter :: cafmodesmput        = 10

  integer, parameter :: cafmodeget          = 11
  integer, parameter :: cafmodesubget       = 12
  integer, parameter :: cafmodemget         = 13
  integer, parameter :: cafmodesubmget      = 14
  integer, parameter :: cafmodemsubget      = 15
  integer, parameter :: cafmodesget         = 16
  integer, parameter :: cafmodessubget      = 17
  integer, parameter :: cafmodeallget       = 18
  integer, parameter :: cafmodesimplesubget = 19
  integer, parameter :: cafmodesmget        = 20
  integer, parameter :: cafmodempisend      = 21
  
  character(len=maxchar), dimension(maxcafmode) :: cafmodename


  integer, parameter :: maxcafhalo = 2

  integer, parameter :: cafmodehalopp = 1
  integer, parameter :: cafmodehalogg = 2

  character(len=maxchar), dimension(maxcafhalo) :: cafhaloname

#ifdef MPI
  integer, parameter :: maxcafsync = 10
#else
  integer, parameter :: maxcafsync =  9
#endif

  integer, parameter :: cafsyncall   =  1
  integer, parameter :: cafsyncpt2pt =  2
  integer, parameter :: cafsyncring  =  3
  integer, parameter :: cafsyncrand  =  4
  integer, parameter :: cafsync3d    =  5
  integer, parameter :: cafsyncpair  =  6
  integer, parameter :: cafsynccrit  =  7
  integer, parameter :: cafsynclock  =  8
  integer, parameter :: cafsyncnull  =  9
  integer, parameter :: cafsyncmpi   = 10

  character(len=maxchar), dimension(maxcafsync) :: cafsyncname

  integer, parameter :: dblesize = 8

  integer :: thisimage, numimages

contains

  subroutine cafinit()

    double precision :: tstart

    integer :: rank, ierr
    logical :: lflag, gflag

    cafmodename(cafmodeput)  = "put"
    cafmodename(cafmodesubput) = "subput"
    cafmodename(cafmodesimplesubput) = "simple subput"
    cafmodename(cafmodeget)  = "get"
    cafmodename(cafmodesubget)  = "subget"
    cafmodename(cafmodesimplesubget)  = "simple subget"
    cafmodename(cafmodemput) = "many put"
    cafmodename(cafmodemget) = "many get"
    cafmodename(cafmodesubmput) = "sub manyput"
    cafmodename(cafmodesubmget) = "sub manyget"
    cafmodename(cafmodemsubput) = "many subput"
    cafmodename(cafmodemsubget) = "many subget"
    cafmodename(cafmodesput) = "strided put"
    cafmodename(cafmodesget) = "strided get"
    cafmodename(cafmodesmput) = "strided many put"
    cafmodename(cafmodesmget) = "strided many get"
    cafmodename(cafmodessubput) = "strided subput"
    cafmodename(cafmodessubget) = "strided subget"
    cafmodename(cafmodeallput) = "all put"
    cafmodename(cafmodeallget) = "all get"
#ifdef MPI
    cafmodename(cafmodempisend) = "MPI Send"
#endif

    cafhaloname(cafmodehalopp)  = "halo put-put"
    cafhaloname(cafmodehalogg)  = "halo get-get"

    cafsyncname(cafsyncall)   = "sync all"
    cafsyncname(cafsyncpt2pt) = "sync images pt2pt"
    cafsyncname(cafsyncring)  = "sync images ring"
    cafsyncname(cafsyncrand)  = "sync images random"
    cafsyncname(cafsync3d)    = "sync images 3d grid"
    cafsyncname(cafsyncpair)  = "sync images pairwise"
    cafsyncname(cafsynccrit)  = "sync critical"
    cafsyncname(cafsynclock)  = "sync lock"
    cafsyncname(cafsyncnull)  = "sync null"
#ifdef MPI
    cafsyncname(cafsyncmpi)   = "sync mpi barrier"
#endif

    thisimage = this_image()
    numimages = num_images()
    
!  Warm up the clock

    tstart = caftime()
    if (tstart < 0.0) write(*,*) 'time = ', tstart
 
#ifdef MPI
! Start MPI

    call MPI_Init(ierr)

! Check we have the appropriate correspondence

    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    lflag = .true.

    if (rank+1 .ne. thisimage) lflag = .false.

    gflag = gand(lflag)

    if (gflag .neqv. .true.) then
      if (thisimage == 1) write(*,*) 'Error! rank and thisimage mismatch'
      stop
    end if
#endif

  end subroutine cafinit


  subroutine cafend()

    logical :: flag
    integer :: ierr

#ifdef MPI
    call mpi_initialized(flag, ierr)

    if (flag .eqv. .true.) then

       call mpi_finalized(flag, ierr)

       if (flag .eqv. .false.) call mpi_finalize(ierr)

    end if
#endif

  end subroutine cafend
  

  subroutine cafput(target, source, disp, count, image)
    
    integer :: disp, count, image

    double precision, dimension(:), codimension[*] :: target
    double precision, dimension(:)                 :: source

    target(disp:disp+count-1)[image] = source(disp:disp+count-1)

  end subroutine cafput


  subroutine cafget(target, source, disp, count, image)
    
    integer :: disp, count, image

    double precision, dimension(:)                 :: target
    double precision, dimension(:), codimension[*] :: source

    target(disp:disp+count-1) = source(disp:disp+count-1)[image]

  end subroutine cafget


  subroutine cafsimpleput(target, source, n, image)
    
    integer :: n, disp, count, image

    double precision, dimension(n), codimension[*] :: target
    double precision, dimension(n)                 :: source

    target(:)[image] = source(:)

  end subroutine cafsimpleput


  subroutine cafsimpleget(target, source, n, image)
    
    integer :: n, disp, count, image

    double precision, dimension(n)                 :: target
    double precision, dimension(n), codimension[*] :: source

    target(:) = source(:)[image]

  end subroutine cafsimpleget


  subroutine cafmanyput(target, source, disp, count, blksize, image)
    
    integer :: disp, count, blksize, image
    integer :: i

    double precision, dimension(:), codimension[*] :: target
    double precision, dimension(:)                 :: source

    do i = 1, count

       target(disp+(i-1)*blksize:disp+i*blksize-1)[image] = &
            source(disp+(i-1)*blksize:disp+i*blksize-1)

!      call cafput(target, source, n, disp+(i-1)*blksize, blksize, image)

    end do

  end subroutine cafmanyput


  subroutine cafmanyget(target, source, disp, count, blksize, image)
    
    integer :: disp, count, blksize, image
    integer :: i

    double precision, dimension(:)                 :: target
    double precision, dimension(:), codimension[*] :: source

    do i = 1, count

       target(disp+(i-1)*blksize:disp+i*blksize-1) = &
            source(disp+(i-1)*blksize:disp+i*blksize-1)[image]

!      call cafget(target, source, n, disp+(i-1)*blksize, blksize, image)

    end do

  end subroutine cafmanyget


  subroutine cafstridedput(target, source, istart, istop, stride, image)
    
    integer :: disp, istart, istop, stride, image

    double precision, dimension(:), codimension[*] :: target
    double precision, dimension(:)                 :: source

    target(istart:istop:stride)[image] = source(istart:istop:stride)

  end subroutine cafstridedput


  subroutine cafstridedget(target, source, istart, istop, stride, image)
    
    integer :: disp, istart, istop, stride, image

    double precision, dimension(:)                 :: target
    double precision, dimension(:), codimension[*] :: source

    target(istart:istop:stride) = source(istart:istop:stride)[image]

  end subroutine cafstridedget


  subroutine cafdosync(cafsynctype, active, neighbours)

    integer :: cafsynctype
    logical :: active
    integer, dimension(:) :: neighbours

    integer :: ierr
    
    select case (cafsynctype)


    case (cafsyncall)

       if (.not. active) then

          write(*,*) 'image ', thisimage, ': invalid active = ', active
          stop

       else

          sync all

       endif

    case (cafsyncmpi)

       if (.not. active) then

          write(*,*) 'image ', thisimage, ': invalid active = ', active
          stop

       else
#ifdef MPI
          call MPI_Barrier(MPI_COMM_WORLD, ierr)
#else
          write(*,*) 'cafdosync: ERROR, MPI not enabled but cafsync = ', &
                      cafsynctype
          stop
#endif

       endif

    case (cafsyncpt2pt, cafsyncring, cafsyncrand, cafsync3d, cafsyncpair)

       if (active) then

          sync images(neighbours)

       end if


    case (cafsyncnull)

    case default

       write(*,*) 'cafsync: invalid type = ', cafsynctype
       stop

    end select

  end subroutine cafdosync


  subroutine cafset2d(x, count, stride, blksize, value, docheck)

    integer :: count, stride, blksize

    double precision, dimension(:,:) :: x
    double precision                 :: value

    logical :: docheck

    double precision, allocatable, dimension(:) :: xvec

    integer :: icount, i, n1, n2


    if (docheck) then
 
       n1 = size(x,1)
       n2 = size(x,2)

       allocate(xvec(n1*n2))

       xvec = reshape(x, (/ n1*n2 /))

       call cafset(xvec, count, stride, blksize, value, docheck)

       x = reshape(xvec, (/ n1, n2 /))

       deallocate(xvec)

    end if

  end subroutine cafset2d


  subroutine cafset(x, count, stride, blksize, value, docheck)

    integer :: count, stride, blksize

    double precision, dimension(:) :: x
    double precision               :: value

    logical :: docheck

    integer :: icount, i

    if (docheck) then

       x(:) = 0.0

       do icount = 1, count

          do i = (icount-1)*stride+1, (icount-1)*stride+blksize

             if (i .lt. 1 .or. i .gt. size(x)) then
                write(*,*) 'cafset: internal error, i = ', i
             end if

             x(i) = value

          end do

       end do

    end if

  end subroutine cafset

  logical function cafcheck2d(x, count, stride, blksize, value, docheck)

    integer :: count, stride, blksize

    double precision, dimension(:,:) :: x
    double precision                 :: value

    logical :: docheck

    double precision, allocatable, dimension(:) :: xvec

    integer :: icount, i, n1, n2


    cafcheck2d = .true.

    if (docheck) then
 
       n1 = size(x,1)
       n2 = size(x,2)

       allocate(xvec(n1*n2))

       xvec = reshape(x, (/ n1*n2 /))

       cafcheck2d = cafcheck(xvec, count, stride, blksize, value, docheck)

       deallocate(xvec)

    end if

  end function cafcheck2d


  logical function cafcheck(x, count, stride, blksize, value, docheck)

    integer :: count, stride, blksize

    double precision, dimension(:) :: x
    double precision               :: value

    logical :: docheck

    integer :: icount, i

    cafcheck = .true.

    if (docheck) then
 
       do icount = 1, count

          do i = (icount-1)*stride+1, (icount-1)*stride+blksize

             if (i .lt. 1 .or. i .gt. size(x)) then
                write(*,*) 'cafcheck: internal error, i = ', i
             end if

             if (x(i) /= value) then

                write(*,*) 'ERROR: image ', thisimage, ', x(', i, ') = ', x(i)
                write(*,*) 'ERROR: image ', thisimage, ', expected x = ', value
                cafcheck = .false.
                return

             end if

          end do

          do i = (icount-1)*stride + blksize + 1, icount*stride
            
             if (i .lt. 1 .or. i .gt. size(x)) then
                write(*,*) 'cafset: internal error, i = ', i
             end if

             if (x(i) /= 0.0) then

                write(*,*) 'ERROR: image ', thisimage, ', x(', i, ') = ', x(i)
                write(*,*) 'ERROR: image ', thisimage, ', expected x = ', 0.0
                cafcheck = .false.
                return

             end if

          end do

       end do

    else

       if (x(1) == 0) then
          write(*,*) 'ERROR: image ', thisimage, ' not expecting x(1) = ', x(1)
          cafcheck = .false.
       end if

    end if

  end function cafcheck

  logical function gand(lvalue)

    logical :: lvalue
    logical, save, codimension[*] :: ltmp

    integer :: image

    ltmp = lvalue

    sync all
    
    gand = .true.

    do image = 1, numimages
       gand = gand .and. ltmp[image]
    end do

    sync all

  end function gand


  subroutine dbcast(dval, root)

    double precision :: dval
    integer :: root

    double precision, save, codimension[*] :: dtmp

    if (thisimage == root) then
       dtmp = dval
    end if

    sync all

    dval = dtmp[root]

    sync all

  end subroutine dbcast


  subroutine cafdummy(x)
    
    double precision, dimension(:) :: x

    x(1) = x(1) + 1

  end subroutine cafdummy

  subroutine delay(n)

  implicit none

  integer :: n
  integer :: i, j

! Need to have a multimplier so n is not too large

  integer, parameter :: nrpt = 100

  double precision :: aaaa
  double precision, save :: bbbb = 0


  aaaa = 0.0

  do i = 1, n
    do j = 1, nrpt
       aaaa = aaaa + mod(i,2)
    end do
  end do

  bbbb = bbbb + aaaa

  if (bbbb < 0) write(*,*) 'bbbb = ', bbbb

end subroutine delay


subroutine getdecomp3d(dims)

  integer, parameter :: ndims = 3
  integer, dimension(ndims) :: dims

  integer :: i, numimages3d

#ifdef MPI
  integer, dimension(ndims) :: tmpdims

  logical :: flag
  integer :: ierr

  call mpi_finalized(flag, ierr)

  if (flag .eqv. .true.) then
     if (thisimage == 1) write(*,*) 'getdecomp3d: error: mpi finalized!'
     stop
  end if

  call mpi_initialized(flag, ierr)

  if (flag .eqv. .false.) call mpi_init(ierr)

  tmpdims(:) = 0

  call mpi_dims_create(numimages, ndims, tmpdims, ierr)

! Swap order re C <-> Fortran

  do i = 1, ndims

    dims(i) = tmpdims(ndims-i+1)

  end do
#else

  select case (numimages)  

  case(   1) 
             dims = [  1,  1,  1 ]
  case(   2)
             dims = [  1,  1,  2 ]
  case(   3)
             dims = [  1,  1,  3 ]
  case(   4)
             dims = [  1,  2,  2 ]
  case(   5)
             dims = [  1,  1,  5 ]
  case(   6)
             dims = [  1,  2,  3 ]
  case(   7)
             dims = [  1,  1,  7 ]
  case(   8)
             dims = [  2,  2,  2 ]
  case(   9)
             dims = [  1,  3,  3 ]
  case(  10)
             dims = [  1,  1, 10 ]
  case(  12)
             dims = [  2,  2,  3 ]
  case(  16)
             dims = [  2,  2,  4 ]
  case(  32)
             dims = [  2,  4,  4 ]
  case(  64)
             dims = [  4,  4,  4 ]
  case( 128)
             dims = [  4,  4,  8 ]
  case( 256)
             dims = [  4,  8,  8 ]
  case( 512)
             dims = [  8,  8,  8 ]
  case(1024)
             dims = [  8,  8, 16 ]
  case(2048)
             dims = [  8, 16, 16 ]
  case(4096)
             dims = [ 16, 16, 16 ]

  case default

    if (thisimage == 1) &
      write(*,*) 'decomp3d: unsupported number of images = ', numimages

    stop

    end select
#endif

  numimages3d = 1

  do i = 1, ndims
    numimages3d = numimages3d * dims(i)
  end do

  if (numimages3d /= numimages) then

    if (thisimage == 1) write(*,*) 'decomp3d: internal error'
    stop

  end if

end subroutine getdecomp3d


subroutine getneighs(me, setsize, neighs, cafsynctype)

  implicit none

  integer :: cafsynctype

  integer :: me, setsize
  integer, dimension(:) :: neighs

  integer, allocatable, dimension(:) :: perm
  
  integer :: n, i, j, partner, tmpneigh

  n = size(neighs)

  if (mod(n,2) /= 0 .and. cafsynctype /= cafsyncpair) then
    write(*,*) 'getneighs: illegal number = ', n
    stop
  end if

  select case (cafsynctype)

  case (cafsync3d)

     call getneighs3d(thisimage, neighs)

  case (cafsyncpair)

     call getneighspair(thisimage, neighs)

  case(cafsyncring, cafsyncrand)

     allocate(perm(setsize))

     neighs(:) = 0
     tmpneigh = 0

     if (cafsynctype == cafsyncrand) then

        call getperm(perm)

     else if (cafsynctype == cafsyncring) then

        do i = 1, setsize

           perm(i) = i

        end do

     end if

     do i = 1, setsize

        do partner = 1, n/2

           j = getpartner(i, partner, setsize)

           if (perm(i) == me) then
              tmpneigh = tmpneigh + 1
              neighs(tmpneigh) = perm(j)
           end if

           if (perm(j) == me) then
              tmpneigh = tmpneigh + 1
              neighs(tmpneigh) = perm(i)
           end if
       
        end do

     end do

     if (tmpneigh /= n) then
        write(*,*) 'getneighs: internal error!'
        stop
     end if

     deallocate(perm)

  case default

     write(*,*) 'Illegal cafsynctype: ', cafsynctype
     stop

  end select

end subroutine getneighs

subroutine getneighspair(me, neighs)

  integer :: me
  integer, dimension(:) :: neighs

  if (size(neighs) /= 1) then
     if (thisimage == 1) write(*,*) 'getneighspair: error - neighs must be 1'
     stop
  end if

  if (me <= numimages/2) then
     neighs(1) = me + numimages/2
  else
     neighs(1) = me - numimages/2
  end if

end subroutine getneighspair


subroutine getneighs3d(me, neighs)

  integer :: me
  integer, dimension(:) :: neighs

  integer, parameter :: ndims = 3

  integer, dimension(ndims) :: dims, imagecoord

  integer :: codimx, codimy, codimz
  integer :: coordx, coordy, coordz
  integer :: upcoordx, upcoordy, upcoordz
  integer :: dncoordx, dncoordy, dncoordz

  integer, allocatable, codimension[:,:,:] :: x

  if (size(neighs) /= 6) then
     if (thisimage == 1) write(*,*) 'getneighs3d: error - neighs must be 6'
     stop
  end if

  call getdecomp3d(dims)

  codimx = dims(1)
  codimy = dims(2)
  codimz = dims(3)
  
  allocate(x[codimx, codimy, *])

  imagecoord(:) = this_image(x)

  coordx = imagecoord(1)
  coordy = imagecoord(2)
  coordz = imagecoord(3)
    
!  general case: upimage = mod(thisimage+imageshift-1, nimages) + 1
!  general case: dnimage = mod(thisimage+nimages-imageshift-1, nimages) + 1

  upcoordx = mod(coordx, codimx) + 1
  upcoordy = mod(coordy, codimy) + 1
  upcoordz = mod(coordz, codimz) + 1

  dncoordx = mod(coordx+codimx-2, codimx) + 1
  dncoordy = mod(coordy+codimy-2, codimy) + 1
  dncoordz = mod(coordz+codimz-2, codimz) + 1

  neighs(1) = image_index(x, [upcoordx, coordy, coordz])
  neighs(2) = image_index(x, [coordx, upcoordy, coordz])
  neighs(3) = image_index(x, [coordx, coordy, upcoordz])
  neighs(4) = image_index(x, [dncoordx, coordy, coordz])
  neighs(5) = image_index(x, [coordx, dncoordy, coordz])
  neighs(6) = image_index(x, [coordx, coordy, dncoordz])

  deallocate(x)
  
end subroutine getneighs3d


integer function getpartner(image, partner, numimages)

  implicit none

  integer :: image, partner, numimages

  getpartner = mod(image+partner-1, numimages) + 1

end function getpartner


subroutine getperm(perm)

  implicit none

  integer, dimension(:) :: perm

  integer :: n

  integer :: i, j, tmp

  double precision :: rng

  n = size(perm,1)

  do i = 1, n
    perm(i) = i
  end do

  do i = 1, n-1

! Put a random element from i:n into i

    call random_number(rng)

    j = i + int(rng*dble(n-i+1))

    tmp = perm(i)
    perm(i) = perm(j)
    perm(j) = tmp

  end do

end subroutine getperm

end module cafcore
