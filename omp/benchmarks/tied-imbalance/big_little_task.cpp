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

int main(){

    int depth = 10;

#pragma omp parallel
#pragma omp single
    {
#pragma omp task
        big_tree(depth);
#pragma omp task
        short_tree(depth);

#pragma omp taskwait

    }

    return 0;
}
