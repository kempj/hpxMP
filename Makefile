all: libopenmp.so
	
libopenmp.so: hpxMP.o
	g++ -shared -Wl,-soname,libopenmp.so -o libopenmp.so hpxMP.o -L/home/jeremy/hpxc/lib -lhpxcd `pkg-config --cflags --libs hpx_application`

hpxMP.o: hpxMP.cpp hpxMP.h
	g++ -fPIC -c hpxMP.cpp -o hpxMP.o -I/home/jeremy/hpxc/include -L/home/jeremy/hpxc/lib -lhpxcd `pkg-config --cflags --libs hpx_application`

clean:
	rm -rf *.o
	rm -rf *.so


run: libopenmp.so
	LD_PRELOAD=./libopenmp.so ./simple-omp/omp-par
