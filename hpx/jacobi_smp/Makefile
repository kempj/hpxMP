
CC=g++

default: jacobi

jacobi: jacobi_hpx.o jacobi.cpp jacobi.hpp
	$(CC) -o jacobi jacobi.cpp jacobi_hpx.o --std=c++11 `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=jacobi
	#/home/jeremy/packages/boost/lib/libboost_program_options.so


jacobi_hpx.o: jacobi_hpx.cpp jacobi.hpp
	$(CC) -c -o jacobi_hpx.o jacobi_hpx.cpp --std=c++11 `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=jacobi_hpx.o 

#/home/jeremy/packages/boost/lib/libboost_program_options.so


clean:
	rm -rf *.o
	rm -rf jacobi

