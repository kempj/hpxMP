all: micro taskbench hpx-startup

1d_stencil:
	clang++ -fopenmp 1d_stencil_3_omp_task.cpp -o omp4 `pkg-config --cflags --libs hpx_application`

lu-hpx: hpx-lu.cpp
	g++  hpx-lu.cpp -o hpx-lu `pkg-config --cflags --libs hpx_application`

lu-hpx-dataflow: lu-hpx-dataflow.cpp lu-utils.o lu-local.o
	g++ -o lu-df lu-hpx-dataflow.cpp lu-utils.o lu-local.o `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=lu-df

lu-utils.o: lu-utils.cpp
	g++ -o lu-utils.o -c lu-utils.cpp `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=lu-utils.o

lu-local.o: lu-local.cpp
	g++ -o lu-local.o -c lu-local.cpp `pkg-config --cflags --libs hpx_application` -liostreams -DHPX_APPLICATION_NAME=lu-local.o

fib: fibonacci.cpp
	g++ -g fibonacci.cpp -o hpx-fib `pkg-config --cflags --libs hpx_application`

micro: micro-bench.cpp
	g++  micro-bench.cpp -o micro `pkg-config --cflags --libs hpx_application`

taskbench: hpx-taskbench.cpp
	g++  hpx-taskbench.cpp -o taskbench `pkg-config --cflags --libs hpx_application`

task-wait: hpx-wait-bench.cpp
	g++  hpx-wait-bench.cpp -o task-wait `pkg-config --cflags --libs hpx_application`

startup: hpx-startup.cpp
	g++  hpx-startup.cpp -o startup `pkg-config --cflags --libs hpx_application`

spawn: timed_task_spawn.cpp
	g++ timed_task_spawn.cpp -o spawn `pkg-config --cflags --libs hpx_application`

chain: task-chain.cpp
	g++ -O3 task-chain.cpp -o chain `pkg-config --cflags --libs hpx_application`

