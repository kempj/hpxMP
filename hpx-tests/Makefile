all: lu-hpx lu-hpx-dataflow

lu-hpx: hpx-lu.cpp
	g++  hpx-lu.cpp -o hpx-lu `pkg-config --cflags --libs hpx_application`

lu-hpx-dataflow: lu-hpx-dataflow.cpp lu-utils.o lu-local.o
	g++ -o lu-df lu-hpx-dataflow.cpp lu-utils.o lu-local.o `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=lu-df

lu-utils.o: lu-utils.cpp
	g++ -o lu-utils.o -c lu-utils.cpp `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=lu-utils.o

lu-local.o: lu-local.cpp
	g++ -o lu-local.o -c lu-local.cpp `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=lu-local.o

exec: exec_test.cpp
	g++ -g exec_test.cpp -o test-exec `pkg-config --cflags --libs hpx_application`

exec-ptr: exec_ptr_test.cpp
	g++ -g exec_ptr_test.cpp -o test-exec-ptr `pkg-config --cflags --libs hpx_application`
