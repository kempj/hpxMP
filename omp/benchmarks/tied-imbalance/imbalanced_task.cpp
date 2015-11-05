#include <iostream>
#include <chrono>
#include <thread>

using std::cout;
using std::endl;


void task_tree(int level, int width, int time) {
    if(level > 0) {
        for(int i = 0; i < width; i++) {
#pragma omp task
            task_tree(level - 1, width, time);
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(time));
#pragma omp taskwait
    }
}

void imbalanced_tree(int level, int width, int time) {
    if(level > 0) {
#pragma omp task

        for(int i = 1; i < width; i++) {
#pragma omp task
            task_tree(1, 2, time);
        }
        task_tree(level - 1, 1, time);
        std::this_thread::sleep_for(std::chrono::milliseconds(time));
#pragma omp taskwait
    }
}

int main(int argc, char **argv){
    int depth = 8;
    int width = 8;
    int short_time = 50;
    int long_time = 5000;

    if(argc > 1) {
        depth = atoi(argv[1]);
    }
    std::chrono::time_point<std::chrono::high_resolution_clock> short_end, long_end;

#pragma omp parallel
#pragma omp single
    {
        auto start = std::chrono::high_resolution_clock::now();
#pragma omp task
        {
            imbalanced_tree(depth, width, long_time);
            long_end = std::chrono::high_resolution_clock::now();
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(long_time));

#pragma omp task
        {
            task_tree(depth, 2, short_time);
            short_end = std::chrono::high_resolution_clock::now();
        }

#pragma omp taskwait
        auto end = std::chrono::high_resolution_clock::now();
        auto       time = std::chrono::duration_cast< std::chrono::milliseconds >(end      -start).count();
        auto short_time = std::chrono::duration_cast< std::chrono::milliseconds >(short_end-start).count();
        auto  long_time = std::chrono::duration_cast< std::chrono::milliseconds >(long_end -start).count();
        cout << "total time = " << time << " milliseconds" << endl;
        cout << "short time = " << short_time << " milliseconds" << endl;
        cout << "long  time = " << long_time << " milliseconds" << endl;

    }

    return 0;
}
