CC=~/openUH/bin/uhcc
all: parallel for

for: omp-for.c
	$(CC) omp-for.c -fopenmp -o omp-for

parallel: omp-par.c
	$(CC) omp-par.c -fopenmp -o omp-par

