!----------------------------------------------------------------------------!
!                                                                            !
!  Fortran Coarray Micro-Benchmark Suite - Version 0.9                       !
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

License
-------

This software is released under the license in "LICENSE.txt".


Introduction
------------

This set of benchmarks aims to measure the performance of various
parallel operations involving Fortran coarrays. These include
point-to-point ("ping-pong") data transfer patterns, synchronisation
patterns and halo-swapping for 3D arrays.


Installation
------------

  o Unpack the tar file.

  o Select the required benchmarks by editing "cafparams.f90".

  o Compile using "make".

The supplied Makefile is configured for the Cray compiler - you will
have to set "FC", "FFLAGS". "LDFLAGS" and "LIBS" appropriately for a
different compiler. Note that the benchmark uses MPI as well as Fortran
coarrays.


Execution
---------

The executable "cafbench" runs stand-alone without any flags or input
files. You will have to launch it as appropriate on your parallel
system, eg on a Cray: "aprun -N <numimages> ./cafbench".


Benchmarks
----------

The benchmark has three separate sections:

  o Point-to-point reports the latency and bandwidth (including any
    synchronisation overheads).

  o Synchronisation reports the overhead by performing calculations with
    and without synchronisation and subtracting the two times.

  o Halo reports the time and bandwidth for regular halo swapping in a
    3D pattern.

In all cases the basic data types are double precision numbers.


Point-to-point notes
--------------------

The point-to-point benchmarks use both remote read and remote write.

All data patterns are characterised by three parameters: count, blksize
and stride. The data transferred is "count" separate blocks each of size
"blksize", separated by "stride". We also print out "ndata" (the amount
of data actually sent, ie count*blksize) and "nextent" which is the
distance between the first and last data items (which is larger than
"ndata" for strided patterns). All data arrays contain double precision
numbers.

The same pattern may often be realised in several different ways (eg
inline or via a subroutine) to test the robustness of the compiler. This
might seem unnecessary, but in the early compiler releases seemingly
similar expressions have given very different performance, eg x(1:ndata)
was much slower than x(:).

The word "many" indicates more than one remote operation or more than
one call to a subroutine (subroutines are indicated by "sub"), although
a good compiler may merge these in a single operation.

Synchronisation is included in the timings. Many repetitions are done to
get sensible results and the sync is also done many times.
Synchronisation can be global (sync all) or point-to-point (sync
images).

Pingpongs are done in three ways:

"Single ping-pong" between images 1 and numimages. All other images are
idle (except that they must call "sync all" for global synchronisation).

"Multiple ping-pong" where _all_ images are active. Image "i" is paired
with image "i+numimages/2"; note this only takes place for even numbers
of images greater than 2. This can give significantly different
bandwidths depending on the choice of synchronisation (see "crossing"
below).

"Multiple crossing ping-pong" which is as above but every other pair
swaps in the opposite direction, ie if image 1 is sending to
1+numimages/2, then image 2 is receiving from 2+numimages/2. This
ensures that we exploit the bidirectional bandwidth. Note that in
practice this is the same as "mutiple" if you use point-to-point
synchronisation: in that case the pairs of images naturally get out of
sync as they contend for bandwidth. However, for global synchronisation
this realises a different pattern from "multiple". This test is really
to explain any differences in "multiple" for different choices of
synchronisation.


The patterns are as follows - all except "MPI Send" are replicated for
get (remote read):

"put"    Contiguous put done inline: x(1:ndata)[image2] = x(1:ndata)

"subput" Contiguous put done via a subroutine with target = source = x and
         disp = 1, count = ndata, ie:
         target(disp:disp+count-1)[image] = source(disp:disp+count-1)

"simple subput"  As above but with simpler arguments to subroutine:
                 target(:)[image] = source(:)

"all put" Arrays are allocated to be of size ndata and simple call is done
          inline. This is like "simple subput" above except there the arrays
          are implicitly resized via a subroutine call. Code is:
          x(:)[image2] = x(:)

"many put"  A contiguous put done as many separate puts of different blksize:
            do i = 1, count
               x(1+(i-1)*blksize:i*blksize)[image2] = &
               x(1+(i-1)*blksize:i*blksize)

"sub manyput" Exactly as "many put" but done in a separate subroutine:
              do i = 1, count
                target(disp+(i-1)*blksize:disp+i*blksize-1)[image] = &
                source(disp+(i-1)*blksize:disp+i*blksize-1)

"many subput" Same pattern but with many separate invocations of "subput":
              do i = 1, count
                call cafput(x, x, 1+(i-1)*blksize, blksize, image1)

"strided put" Strided data done inline in the code:
              x(1:nextent:stride)[image2] = x(1:nextent:stride)

"strided subput" As above but done via a subroutine:
              target(istart:istop:stride)[image] = source(istart:istop:stride)


"strided many put" The most complex pattern: strided but with blocks
                   larger than a single unit. Pattern is a block of data,
                   followed by a gap of the same size, repeated:
                   do i = 1, count
                     x(1+2*(i-1)*blksize:(2*i-1)*blksize)[image2] = &
                     x(1+2*(i-1)*blksize:(2*i-1)*blksize)

                   This pattern is a useful measurement in cases where the
                   compiler vectorises "many put" into a single put of
                   size ndata.

"MPI Send" A regular MPI ping-pong with no coarray synchronisation, done as
           a sanity check for the coarray performance numbers.


Synchronisation notes
---------------------

The different synchronisation types are:

  o sync all: a simple call to "sync all".

  o sync mpi barrier: MPI call for comparison with "sync all" above.

  o sync images pairwise: each image calls "sync images" with a single
    neighbour; images are paired up in the same pattern as for
    "Multiple ping-pong" above.

  o sync images random: each images calls "sync images" with N
    neighbours chosen randomly (to ensure that they all match up I
    actually set up a simple ring pattern then randomly permute). N is
    chosen as 2, 4, 6, ... syncmaxneigh, capped if this starts to
    exceed the total number of images. Default syncmaxneigh is 12.

  o sync images ring: each image calls "sync images" with N neighbours
    paired as image +/- 1, image +/- 2 ...  image +/- syncmaxneigh/2
    with periodic boundary conditions.

  o sync images 3d grid: each image calls "sync images" with 6
    neighbours which are chosen as the up and down neighbours in all
    directions in a 3D cartesian grid (with periodic boundaries). The
    dimensions of the 3D grid are selected via a call to MPI_Cart_dims
    (suitably reversed for Fortran indexing). This is precisely the
    synchronisation pattern used in the subsequent halo benchmark.

  o sync lock: all images lock a variable on image 1.

  o sync critical: all images execute a critical region.

Note that in all of these the time for some computation (a simple
delay loop) is compared to the time for the computation plus
synchronisation, and these are subtracted to get the synchronisation
overhead.
