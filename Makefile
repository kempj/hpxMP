all: libopenmp.so
	
libopenmp.so: hpxMP.o
	g++ -shared -Wl,-soname,libopenmp.so -o libopenmp.so hpxMP.o

hpxMP.o: hpxMP.cpp hpxMP.h
	g++ -fPIC -c hpxMP.cpp -o hpxMP.o

clean:
	rm -rf *.o
	rm -rf *.so


run: libopenmp.so
	LD_PRELOAD=./libopenmp.so ../simple-omp/a.out
