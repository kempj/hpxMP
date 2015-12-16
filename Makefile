CC=g++
cc=gcc
HPX_BUILD_TYPE=hpx_application
OPT= -O3
FLAGS= -DOMP_COMPLIANT $(OPT)
CFLAGS=

all: libiomp5.so
	

.PHONY: libiomp5.so
libiomp5.so:
	cd src; make; cp libiomp5.so ..

.PHONY: tests tests-omp tests-omp-clang tests-omp-UH tests-omp-icc
tests: tests-omp

tests-omp: tests-omp-clang tests-omp-UH tests-omp-icc

tests-omp-clang: libiomp5.so
	cd omp/tests; make CC=clang RT=libiomp5.so

tests-omp-icc: libiomp5.so
	cd omp/tests; make CC=icc RT=libiomp5.so


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
