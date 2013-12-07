all: libopenmp.so
	
hpxMPopt.o: hpxMP.cpp hpxMP.h
	g++ -g -O3 -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs hpx_application`

opt: hpxMPopt.o
	g++ -O3 -shared -Wl,-soname,libopenmp.so -o libopenmp.so hpxMP.o `pkg-config --cflags --libs hpx_application`

libopenmp.so: hpxMP.o hpx_runtime.o
	g++ -shared -Wl,-soname,libopenmp.so -o libopenmp.so hpxMP.o hpx_runtime.o `pkg-config --cflags --libs hpx_application`

hpx_runtime.o: hpx_runtime.cpp
	g++ -fPIC -c hpx_runtime.cpp -o hpx_runtime.o `pkg-config --cflags --libs hpx_application`

hpxMP.o: hpxMP.cpp hpxMP.h
	g++ -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs hpx_application`

clean:
	rm -rf *.o
	rm -rf *.so

tests: libopenmp.so par-test for-test par-nested-test barrier-test single-test master-test

par-test:
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-par
for-test:
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-for
par-nested-test:
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-nested-par
barrier-test: libopenmp.so ./omp-tests/omp-barrier
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-barrier

single-test: libopenmp.so ./omp-tests/omp-single
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-single

master-test: libopenmp.so ./omp-tests/omp-master
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-master


not-working: libopenmp.so task-test

task-test: libopenmp.so ./omp-tests/omp-task
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-task

fib-test: libopenmp.so ./omp-tests/omp-fib
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-fib

#first number should be 100x the second, to keep the blocksize the same
#blocksize = 100x100 : 1000/10 = 100
lu-test: libopenmp.so ./omp-tests/omp-lu
	LD_PRELOAD=./libopenmp.so ./omp-tests/omp-lu 1000 10

