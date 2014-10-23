CC=g++-4.8
all: libopenmp.so.1
	

libiomp5.so: intel_rt.o hpx_runtime.o
	$(CC) -g -shared -Wl,-x -Wl,-soname=libiomp5.so,--version-script=exports_so.txt -o libiomp5.so intel_rt.o hpx_runtime.o -L. `pkg-config --cflags --libs hpx_application`

intel_rt.o: intel_hpxMP.cpp intel_hpxMP.h
	$(CC) -g -fPIC -c intel_hpxMP.cpp -o intel_rt.o `pkg-config --cflags --libs hpx_application`


debug: hpxMPd.o hpx_runtimed.o
	$(CC) -g -shared -Wl,-soname,libopenmp.so.1 -o libopenmp.so.1 hpxMPd.o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

hpx_runtimed.o: hpx_runtime.cpp
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

hpxMPd.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMPd.o `pkg-config --cflags --libs hpx_application_debug`

libopenmp.so.1: hpxMP.o hpx_runtime.o loop_schedule.o loop_data.o
	$(CC) -g -shared -Wl,-x -Wl,-soname=libopenmp.so.1,--version-script=libopenmp.vs -o libopenmp.so.1 hpxMP.o hpx_runtime.o loop_schedule.o loop_data.o -L. `pkg-config --cflags --libs hpx_application`

hpx_runtime.o: hpx_runtime.cpp hpx_runtime.h 
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtime.o `pkg-config --cflags --libs hpx_application`

hpxMP.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs hpx_application`

loop_schedule.o: loop_schedule.cpp loop_schedule.h
	$(CC) -g -fPIC -c loop_schedule.cpp -o loop_schedule.o `pkg-config --cflags --libs hpx_application`

loop_data.o: loop_data.cpp loop_data.h
	$(CC) -g -fPIC -c loop_data.cpp -o loop_data.o `pkg-config --cflags --libs hpx_application`


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
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-fib task 24 0

#first number should be 100x the second, to keep the blocksize the same
#blocksize = 100x100 : 1000/10 = 100
lu-test: libopenmp.so.1 ./omp-tests/omp-lu
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/omp-lu 1000 10

epcc: schedbench-test taskbench-test

syncbench-test: libopenmp.so.1 syncbench
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/epcc/omp_v3/C/syncbench

schedbench-test: libopenmp.so.1 schedbench
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/epcc/omp_v3/C/schedbench

taskbench-test: libopenmp.so.1 taskbench
	LD_PRELOAD=./libopenmp.so.1 ./omp-tests/epcc/omp_v3/C/taskbench

syncbench:
	cd omp-tests/epcc/omp_v3/C; make syncbench

schedbench:
	cd omp-tests/epcc/omp_v3/C; make schedbench

taskbench:
	cd omp-tests/epcc/omp_v3/C; make taskbench

buildTests:
	cd omp-tests; make
