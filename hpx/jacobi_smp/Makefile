
CC=g++

default: jacobi

jacobi: jacobi_hpx.o jacobi.cpp jacobi.hpp
	$(CC) -O3 -o jacobi jacobi.cpp jacobi_hpx.o `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=jacobi


jacobi_hpx.o: jacobi_hpx.cpp jacobi.hpp
	$(CC) -O3 -o jacobi_hpx.o jacobi_hpx.cpp `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=jacobi_hpx.o


clean:
	rm -rf *.o
	rm -rf jacobi

