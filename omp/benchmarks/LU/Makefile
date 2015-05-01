CC=g++

default: lu-dataflow

lu-dataflow: lu-dataflow.cpp lu-local.o lu-utils.o
	$(CC) -fopenmp --std=c++11 lu-dataflow.cpp lu-local.o lu-utils.o -o lu-dataflow

serial-lu: lu-dataflow.cpp lu-local.o lu-utils.o
	$(CC) --std=c++11 lu-dataflow.cpp lu-local.o lu-utils.o -o serial-lu

lu-local.o: lu-local.h lu-local.cpp
	$(CC) -c lu-local.cpp -std=c++11 -o lu-local.o

lu-utils.o: lu-utils.h lu-utils.cpp
	$(CC) -c lu-utils.cpp -std=c++11 -o lu-utils.o

clean:
	rm -f *.o
	rm -f serial-lu
	rm -f lu-dataflow
