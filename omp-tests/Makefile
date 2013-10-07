CC=~/openUH/bin/uhcc
all: parallel for nested-parallel task barrier

for: omp-for.c
	$(CC) omp-for.c -fopenmp -o omp-for

parallel: omp-par.c
	$(CC) omp-par.c -fopenmp -o omp-par

nested-parallel: omp-nested-par.c
	$(CC) omp-nested-par.c -fopenmp -o omp-nested-par

task: omp-task.c
	$(CC) omp-task.c -fopenmp -o omp-task

barrier: omp-barrier.c
	$(CC) omp-barrier.c -fopenmp -o omp-barrier

single: omp-single.c
	$(CC) omp-single.c -fopenmp -o omp-single

master: omp-master.c
	$(CC) omp-master.c -fopenmp -o omp-master
