#include <iostream>
#include <chrono>
#include <thread>

using std::cout;
using std::endl;
int long_time = 5000;
int short_time = 50;

void big_tree(int level){
    if(level > 0) {
#pragma omp task
        big_tree(level - 1);
#pragma omp task
        big_tree(level - 1);

        std::this_thread::sleep_for(std::chrono::milliseconds(long_time));
#pragma omp taskwait
    }
}

void short_tree(int level) {
    if(level > 0) {
#pragma omp task
        short_tree(level - 1);
#pragma omp task
        short_tree(level - 1);

        std::this_thread::sleep_for(std::chrono::milliseconds(short_time));
#pragma omp taskwait
    }
}

int main(int argc, char **argv){
    int depth = 10;
    if(argc > 1) {
        depth = atoi(argv[1]);
    }

#pragma omp parallel
#pragma omp single
    {
         auto start = std::chrono::high_resolution_clock::now();
#pragma omp task
        big_tree(depth);
#pragma omp task
        short_tree(depth);

#pragma omp taskwait
        auto end = std::chrono::high_resolution_clock::now();
        auto time = std::chrono::duration_cast< std::chrono::milliseconds >(end-start).count();
        cout << "total time = " << time << " milliseconds" << endl;

    }

    return 0;
}
