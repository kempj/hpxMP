#include <chrono>
#include <cstdlib>
#include <iostream>
#include <vector>

using std::vector;
using std::cout;
using std::endl;
using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::nanoseconds;

void delay(int nanosec_delay) {
    auto t1 = high_resolution_clock::now();
    auto t2 = high_resolution_clock::now();
    while(true) {
        if( duration_cast<nanoseconds> (t2-t1).count() > nanosec_delay) {
            break;
        }
        t2 = high_resolution_clock::now();
    }
}
int main(int argc, char ** argv) {

    int num_tasks = 500000;
    int delay_time = 50000;
    if(argc > 1)
        num_tasks = atoi(argv[1]);
    if(argc > 2)
        delay_time = atoi(argv[2]);
    int val = 0;
    auto t1 = high_resolution_clock::now();


#pragma omp parallel
{
#pragma omp single
{
    for(int i = 0; i < num_tasks; i++) {
#pragma omp task
        delay(delay_time);

    }
#pragma omp taskwait
}
}
    auto t2 = high_resolution_clock::now();
    cout << "val = " << val << endl;
    
    auto total = duration_cast<nanoseconds> (t2-t1).count();
    cout << "total time = " << total << endl;

    return 0;
}

