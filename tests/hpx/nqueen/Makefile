
CC=g++

default: nqueen

nqueen: nqueen.cpp nqueen.hpp
	$(CC) -o nqueen nqueen.cpp `pkg-config --cflags --libs hpx_application` -DHPX_APPLICATION_NAME=nqueen


clean:
	rm -rf *.o
	rm -rf nqueen 

