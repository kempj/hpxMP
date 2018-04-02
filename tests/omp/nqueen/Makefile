
CC=g++

default: nqueen

nqueen: nqueen.cpp nqueen.hpp
	$(CC) -o nqueen nqueen.cpp --std=c++11 `pkg-config --cflags --libs hpx_application` -DHPX_APPLICATION_NAME=nqueen


clean:
	rm -rf *.o
	rm -rf nqueen 

