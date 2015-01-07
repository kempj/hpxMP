CC=~/openUH/bin/uhcc
all: parallel for nested-parallel task barrier master single par-for

for: omp-for.c
	$(CC) -g omp-for.c -fopenmp -o omp-for

for-dec: omp-for-decrement.c
	$(CC) -g omp-for-decrement.c -fopenmp -o omp-for-decrement

par-for: omp-par-for.c
	$(CC) -g omp-par-for.c -fopenmp -o omp-par-for

dyn-for: omp-for-dynamic.c
	$(CC) -g omp-for-dynamic.c -fopenmp -o dyn-for

parallel: omp-par.c
	$(CC) -g omp-par.c -fopenmp -o omp-par

nested-parallel: omp-nested-par.c
	$(CC) -g omp-nested-par.c -fopenmp -o omp-nested-par

task: omp-task.c
	$(CC) -g omp-task.c -fopenmp -o omp-task

fib: omp-fib.c
	$(CC) -g omp-fib.c -fopenmp -o omp-fib

fib-opt: omp-fib.c
	$(CC) omp-fib.c -O3 -fopenmp -o omp-fib-O3

barrier: omp-barrier.c
	$(CC) -g omp-barrier.c -fopenmp -o omp-barrier

single: omp-single.c
	$(CC) -g omp-single.c -fopenmp -o omp-single

master: omp-master.c
	$(CC) -g omp-master.c -fopenmp -o omp-master

reduction: omp-reduction.c
	$(CC) -g omp-reduction.c -fopenmp -o omp-reduction

lu: omp-lu.c
	$(CC) -g omp-lu.c -fopenmp -o omp-lu
	
thrdprvt: omp-thrdprvt.c
	$(CC) -g omp-thrdprvt.c -fopenmp -o omp-thrdprvt

fort: test.f90
	uhf90 -g -mp ./test.f90 -o ftest

ftest: fort
	LD_PRELOAD=../libopenmp.so.1 ./ftest

hpxMPfib: hpxMP-fib.cpp
	g++-4.8 -g -I.. -L..  hpxMP-fib.cpp -o hpxMPfib `pkg-config --cflags --libs hpx_application` -lopenmp

