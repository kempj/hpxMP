all: strassen.c strassen.h
	~/openUH/bin/opencc -fopenmp -g strassen.c -o omp-strassen

whirl: strassen.c strassen.h
	~/openUH/bin/opencc -fopenmp strassen.c -o whirl-strassen -CLIST:emit_nested_pu:before_cg
