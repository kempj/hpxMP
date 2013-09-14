all: parallel for

for: omp-for.c
	~/openUH/bin/uhCC omp-for.c -fopenmp -o omp-for

parallel: omp-par.c
	~/openUH/bin/uhcc omp-par.c -fopenmp -o omp-par
