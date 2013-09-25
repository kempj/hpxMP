CC=~/openUH/bin/uhcc
all: parallel for nested-parallel

for: omp-for.c
	$(CC) omp-for.c -fopenmp -o omp-for

parallel: omp-par.c
	$(CC) omp-par.c -fopenmp -o omp-par

nested-parallel: omp-nested-par.c
	$(CC) omp-nested-par.c -fopenmp -o omp-nested-par

