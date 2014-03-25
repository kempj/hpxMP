CC=~/openUH/bin/uhcc
all: parallel for nested-parallel task barrier master single

for: omp-for.c
	$(CC) omp-for.c -fopenmp -o omp-for

par-for: omp-par-for.c
	$(CC) omp-par-for.c -fopenmp -o omp-par-for

parallel: omp-par.c
	$(CC) omp-par.c -fopenmp -o omp-par

nested-parallel: omp-nested-par.c
	$(CC) omp-nested-par.c -fopenmp -o omp-nested-par

task: omp-task.c
	$(CC) omp-task.c -fopenmp -o omp-task

fib: omp-fib.c
	$(CC) omp-fib.c -fopenmp -o omp-fib

barrier: omp-barrier.c
	$(CC) omp-barrier.c -fopenmp -o omp-barrier

single: omp-single.c
	$(CC) omp-single.c -fopenmp -o omp-single

master: omp-master.c
	$(CC) omp-master.c -fopenmp -o omp-master

reduction: omp-reduction.c
	$(CC) omp-reduction.c -fopenmp -o omp-reduction

lu: omp-lu.c
	$(CC) omp-lu.c -fopenmp -o omp-lu
	
thrdprvt: omp-thrdprvt.c
	$(CC) omp-thrdprvt.c -fopenmp -o omp-thrdprvt

fort: test.f90
	uhf90 -g -mp ./test.f90 -o ftest

ftest: fort
	LD_PRELOAD=../libopenmp.so.1 ./ftest
