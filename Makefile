CC=g++
cc=gcc
#most work is done with 4.8
HPX_BUILD_TYPE=hpx_application
FLAGS=

all: libiomp5.so
	

libiomp5.so: intel_rt.o hpx_runtime.o loop_schedule.o kmp_atomic.o asm_functions.o
	$(CC) -shared -Wl,-x -Wl,-soname=libiomp5.so,--version-script=exports_so.txt -o libiomp5.so intel_rt.o loop_schedule.o kmp_atomic.o hpx_runtime.o -L. `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`# -lffi

intel_rt.o: intel_hpxMP.cpp intel_hpxMP.h
	$(CC) -fPIC -c intel_hpxMP.cpp -o intel_rt.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)` 

kmp_atomic.o: kmp_atomic.cpp kmp_atomic.h
	$(CC) -I ./ -D USE_ITT_BUILD -D NDEBUG -D KMP_ARCH_STR="\"Intel(R) 64\"" -D _GNU_SOURCE -D _REENTRANT -D KMP_USE_ASSERT -D BUILD_I8 -D BUILD_TV -D KMP_LIBRARY_FILE=\"libiomp5.so\" -D KMP_VERSION_MAJOR=5 -D CACHE_LINE=64 -D KMP_ADJUST_BLOCKTIME=1 -D BUILD_PARALLEL_ORDERED -D KMP_ASM_INTRINS -D KMP_USE_INTERNODE_ALIGNMENT=0 -D KMP_USE_VERSION_SYMBOLS -D USE_LOAD_BALANCE -D USE_CBLKDATA -D GUIDEDLL_EXPORTS -D KMP_GOMP_COMPAT -D KMP_NESTED_HOT_TEAMS -D KMP_USE_ADAPTIVE_LOCKS=1 -D KMP_DEBUG_ADAPTIVE_LOCKS=0 -D KMP_STATS_ENABLED=0 -D OMP_50_ENABLED=0 -D OMP_41_ENABLED=0 -D OMP_40_ENABLED=1 -D USE_ITT_NOTIFY=1 -D INTEL_ITTNOTIFY_PREFIX=__kmp_itt_ -D KMP_TDATA_GTID -c -fPIC -Wsign-compare -o kmp_atomic.o kmp_atomic.cpp `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

asm_functions.o: asm_functions.s
	$(cc) -c -x assembler-with-cpp -o asm_functions.o asm_functions.s 

libopenmp.so.1: FLAGS += -DBUILD_UH
libopenmp.so.1: hpxMP.o uh-hpx_runtime.o 
	$(CC) -g -shared -Wl,-x -Wl,-soname=libopenmp.so.1,--version-script=libopenmp.vs -o libopenmp.so.1 hpxMP.o uh-hpx_runtime.o -L. `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

uh-hpx_runtime.o: hpx_runtime.cpp hpx_runtime.h 
	$(CC) -g $(FLAGS) -fPIC -c hpx_runtime.cpp -o uh-hpx_runtime.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

hpx_runtime.o: hpx_runtime.cpp hpx_runtime.h 
	$(CC) -g -fPIC -c hpx_runtime.cpp -o hpx_runtime.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)` 

hpxMP.o: hpxMP.cpp hpxMP.h
	$(CC) -g $(FLAGS) -fPIC -c hpxMP.cpp -o hpxMP.o `pkg-config --cflags --libs $(HPX_BUILD_TYPE)`

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
