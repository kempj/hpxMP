#include <iostream>
#include <hpx/hpx_main.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_start.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/util/static.hpp>
#include <hpx/include/lcos.hpp>
#include <hpx/include/thread_executors.hpp>

using hpx::async;
using std::vector;
using hpx::lcos::future;
using std::cout;
using std::endl;
using hpx::threads::executors::local_priority_queue_executor;


void print_ints(int i, int j) {
    cout << " i = " << i << ", j = " << j << endl;
}

void thread_setup(int i, local_priority_queue_executor exec) {
    vector<hpx::lcos::future<void>> tasks;
    for(int j = 0; j < 5; j++) {
        tasks.push_back(async(exec, print_ints, i , j));
    }
}

int main() {
    vector<hpx::lcos::future<void>> threads;
    {
        local_priority_queue_executor exec;
        for(int i = 0; i < 8; i++) {
            threads.push_back(async(thread_setup, i, exec));
        }
        hpx::wait_all(threads);
        cout << "All done" << endl;
    }
    return 0;
}
