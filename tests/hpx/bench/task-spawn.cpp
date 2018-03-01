#include <chrono>

#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>
#include <hpx/include/lcos.hpp>

#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/format.hpp>

#include <hpx/parallel/executors/thread_pool_executors.hpp>

using hpx::lcos::future;
using hpx::dataflow;
using hpx::util::unwrapping;

using std::vector;
using std::cout;
using std::endl;
using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::nanoseconds;

using hpx::threads::executors::local_priority_queue_executor;


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


int hpx_main(int argc, char ** argv) {

    int64_t num_tasks1 = 1000;
    int64_t num_tasks2 = 500;
    int64_t delay_time = 50000;
    if(argc > 1)
        delay_time = atoi(argv[1]);
    if(argc > 2)
        num_tasks1 = atoi(argv[2]);
    if(argc > 3)
        num_tasks2 = atoi(argv[3]);

    int64_t num_tasks = num_tasks1 * num_tasks2;
    cout << "spawning " << num_tasks << " tasks, with a delay of " << delay_time << endl;


    auto t1 = high_resolution_clock::now();
    {
        local_priority_queue_executor exec;
        t1 = high_resolution_clock::now();
        for(int i = 0; i < num_tasks; i++) {
            hpx::apply(exec, delay, delay_time);
            //delay(delay_time);
        }
    }

    auto t2 = high_resolution_clock::now();
    
    auto total = duration_cast<nanoseconds> (t2-t1).count();

    int64_t total_time = num_tasks * delay_time;

    int64_t num_threads = hpx::threads::hardware_concurrency();
    int64_t theory = total_time / num_threads; 
    cout << "theory time = " << theory    / 1000000.0 << " ms (" << total_time << " / " << num_threads << " threads)" << endl;
    cout << "spawn time  = " << total     / 1000000.0 << " ms : " << (    total - theory) / 1000000.0 << " ms overhead " << endl;

    return hpx::finalize();
}

int main(int argc, char ** argv) {

    return hpx::init(argc, argv);
}
