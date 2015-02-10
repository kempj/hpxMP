CC=g++-4.8
HPX_BUILD_TYPE=hpx_application

all: libiomp5.so
	

libiomp5.so: intel_rt.o hpx_runtime.o loop_schedule.o 
	$(CC) -g -shared -Wl,-x -Wl,-soname=libiomp5.so,--version-script=exports_so.txt -o libiomp5.so intel_rt.o loop_schedule.o  hpx_runtime.o -L. `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

intel_rt.o: intel_hpxMP.cpp intel_hpxMP.h
	$(CC) -g -fPIC -c intel_hpxMP.cpp -o intel_rt.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

libopenmp.so.1: hpxMP.o hpx_runtime.o 
	$(CC) -g -shared -Wl,-x -Wl,-soname=libopenmp.so.1,--version-script=libopenmp.vs -o libopenmp.so.1 hpxMP.o hpx_runtime.o -L. `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

hpx_runtime.o: hpx_runtime.cpp hpx_runtime.h 
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtime.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

hpxMP.o: hpxMP.cpp hpxMP.h
	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

loop_schedule.o: loop_schedule.cpp loop_schedule.h
	$(CC) -g -fPIC -c loop_schedule.cpp -o loop_schedule.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

.PHONY: tests tests-omp tests-omp-clang tests-omp-UH tests-omp-icc
tests: tests-omp

tests-omp: tests-omp-clang tests-omp-UH tests-omp-icc

tests-omp-clang: libiomp5.so
	cd omp/tests; make CC=clang RT=libiomp5.so

tests-omp-icc: libiomp5.so
	cd omp/tests; make CC=icc RT=libiomp5.so

tests-omp-UH: libopenmp.so.1
	cd omp/tests; make CC=uhcc RT=libopenmp.so.1

#.PHONY: debug
debug: HPX_BUILD_TYPE = hpx_application_debug
debug: libiomp5.so
#debug:  intel_rt.o hpx_runtime.o loop_schedule.o
#	$(CC) -g -shared -Wl,-soname,libiomp5.so,--version-script=exports_so.txt -o libiomp5.so intel_rt.o hpx_runtime.o loop_schedule.o  `pkg-config --cflags --libs $(HPX_BUILD_TYPE)_debug`

#hpx_runtimed.o: hpx_runtime.cpp
#	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtimed.o `pkg-config --cflags --libs hpx_application_debug`

#hpxMPd.o: hpxMP.cpp hpxMP.h
#	$(CC) -g -fPIC -c hpxMP.cpp -o hpxMPd.o `pkg-config --cflags --libs hpx_application_debug`


.PHONY: clean
clean:
	rm -rf *.o
	rm -rf *.so
	rm -rf *.so.1
