CC=g++-4.8
all: libopenmp.so.1
	

libiomp5.so: intel_rt.o hpx_runtime.o loop_schedule.o 
	$(CC) -g -shared -Wl,-x -Wl,-soname=libiomp5.so,--version-script=exports_so.txt -o libiomp5.so intel_rt.o loop_schedule.o  hpx_runtime.o -L. `pkg-config --cflags --libs hpx_application`

intel_rt.o: intel_hpxMP.cpp intel_hpxMP.h
	$(CC) -g -fPIC -c intel_hpxMP.cpp -o intel_rt.o `pkg-config --cflags --libs hpx_application`


debug: hpxMPd.o hpx_runtimed.o
	$(CC) -g -shared -Wl,-soname,libopenmp.so.1 -o libopenmp.so.1 hpxMPd.o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

hpx_runtimed.o: hpx_runtime.cpp
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

hpxMPd.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMPd.o `pkg-config --cflags --libs hpx_application_debug`

libopenmp.so.1: hpxMP.o hpx_runtime.o 
	$(CC) -g -shared -Wl,-x -Wl,-soname=libopenmp.so.1,--version-script=libopenmp.vs -o libopenmp.so.1 hpxMP.o hpx_runtime.o -L. `pkg-config --cflags --libs hpx_application`

hpx_runtime.o: hpx_runtime.cpp hpx_runtime.h 
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtime.o `pkg-config --cflags --libs hpx_application`

hpxMP.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs hpx_application`

loop_schedule.o: loop_schedule.cpp loop_schedule.h
	$(CC) -g -fPIC -c loop_schedule.cpp -o loop_schedule.o `pkg-config --cflags --libs hpx_application`

.PHONY: clean tests omp-tests
clean:
	rm -rf *.o
	rm -rf *.so
	rm -rf *.so.1

#hpx-tests hybrid-tests
tests: omp-tests 

omp-tests: 
	cd omp/tests; make CC=clang RT=libiomp5.so
	cd omp/tests; make CC=uhcc RT=libopenmp.so.1


