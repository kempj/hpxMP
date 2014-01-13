CC=g++-4.8
all: libopenmp.so.1
	

debug: hpxMPd.o hpx_runtimed.o
	$(CC) -g -shared -Wl,-soname,libopenmp.so.1 -o libopenmp.so.1 hpxMPd.o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

hpxMPopt.o: hpxMP.cpp hpxMP.h
	$(CC) -g -O3 -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs hpx_application`

hpx_runtimed.o: hpx_runtime.cpp
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

hpxMPd.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMPd.o `pkg-config --cflags --libs hpx_application_debug`

opt: hpxMPopt.o
	$(CC) -O3 -shared -Wl,-soname,libopenmp.so.1 -o libopenmp.so.1 hpxMP.o `pkg-config --cflags --libs hpx_application`

libopenmp.so.1: hpxMP.o hpx_runtime.o
	$(CC) -g -shared -Wl,-soname,libopenmp.so.1 -o libopenmp.so.1 hpxMP.o hpx_runtime.o `pkg-config --cflags --libs hpx_application`

hpx_runtime.o: hpx_runtime.cpp
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtime.o `pkg-config --cflags --libs hpx_application`

hpxMP.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs hpx_application`

clean:
	rm -rf *.o
	rm -rf *.so
	rm -rf *.so.1

tests: libopenmp.so.1 par-test for-test par-nested-test barrier-test single-test master-test par-for-test

par-for-test:
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-par-for

par-test:
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-par
for-test:
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-for
par-nested-test:
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-nested-par
barrier-test: libopenmp.so.1 ./omp-tests/omp-barrier
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-barrier

single-test: libopenmp.so.1 ./omp-tests/omp-single
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-single

master-test: libopenmp.so.1 ./omp-tests/omp-master
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-master


not-working: libopenmp.so.1 task-test

task-test: libopenmp.so.1 ./omp-tests/omp-task
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-task

fib-test: libopenmp.so.1 ./omp-tests/omp-fib
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-fib

#first number should be 100x the second, to keep the blocksize the same
#blocksize = 100x100 : 1000/10 = 100
lu-test: libopenmp.so.1 ./omp-tests/omp-lu
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-lu 1000 10

