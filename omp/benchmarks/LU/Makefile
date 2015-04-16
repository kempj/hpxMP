CC=clang
all: lu-dep lu-task

lu-dep: lu-depends.c
	$(CC) -O3 -fopenmp -o lu-dep lu-depends.c

lu-task: omp-task.c
	$(CC) -O3 -fopenmp -o lu-task omp-task.c
