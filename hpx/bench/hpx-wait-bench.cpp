
#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>
#include <hpx/util/unwrapped.hpp>


#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/format.hpp>

#include <atomic>

using hpx::lcos::shared_future;
using hpx::lcos::future;

using std::atomic;
using std::vector;
using std::cout;
using std::endl;

int delay_length;
atomic<int> task_counter{0};


void placeholder_task() {
    float a = 0.;
    for(int i = 0; i < delay_length; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
}
void wrapper_task() {
    placeholder_task();
    task_counter--;
}

uint64_t task_spawn_wait(int total_tasks) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> tasks;
    tasks.reserve(total_tasks);
    for(int i = 0; i < total_tasks; i++) {
        tasks.push_back(hpx::async(placeholder_task));
    }
    hpx::wait_all(tasks);
    return hpx::util::high_resolution_clock::now() - start;
}

uint64_t task_spawn_count(int total_tasks) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    task_counter = 0;
    for(int i = 0; i < total_tasks; i++) {
        task_counter++;
        hpx::async(wrapper_task);
    }
    while(task_counter > 0) {
        hpx::this_thread::yield();
    }
    return hpx::util::high_resolution_clock::now() - start;
}

uint64_t task_apply_count(int total_tasks) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    task_counter = 0;
    for(int i = 0; i < total_tasks; i++) {
        task_counter++;
        hpx::apply(wrapper_task);
    }
    while(task_counter > 0) {
        hpx::this_thread::yield();
    }
    return hpx::util::high_resolution_clock::now() - start;
}

// Nested tasking: 

void nested_wait_spawner(int num_tasks) {
    vector<future<void>> tasks;
    tasks.reserve(num_tasks);
    for(int i = 0; i < num_tasks; i++) {
        tasks.push_back(hpx::async(placeholder_task));
    }
    hpx::wait_all(tasks);
    //return hpx::when_all(tasks);
}

uint64_t nested_task_wait(int level1, int level2) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> tasks;
    tasks.reserve(level1);
    for(int i = 0; i < level1; i++) {
        tasks.push_back(hpx::async(nested_wait_spawner, level2));
    }
    hpx::wait_all(tasks);
    return hpx::util::high_resolution_clock::now() - start;
}

void nested_spawner(int num_tasks) {
    for(int i = 0; i < num_tasks; i++) {
        task_counter++;
        hpx::apply(wrapper_task);
    }
    task_counter--;
}
uint64_t nested_task_apply_count(int level1, int level2) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    task_counter = 0;
    for(int i = 0; i < level1; i++) {
        task_counter++;
        hpx::apply(nested_spawner, level2);
    }
    while(task_counter > 0) {
        hpx::this_thread::yield();
    }
    return hpx::util::high_resolution_clock::now() - start;
}

int hpx_main(boost::program_options::variables_map& vm) {
    int delay_length = vm["delay_length"].as<int>();
    int num_threads = hpx::get_os_thread_count();
    int total_tasks = num_threads * vm["task_count"].as<int>();
    int nesting1 = 16;
    int nesting2 = total_tasks/nesting1;

    //cout << "time for wait_all  = " << task_spawn_wait(total_tasks) << endl;
    //cout << "time for count     = " << task_spawn_count(total_tasks) << endl;
    
    cout << "apply time (" << total_tasks << ") = " << task_apply_count(total_tasks) << endl;

    cout << "nested-wait(" << nesting1 << ", " << nesting2  << ") = " 
        << nested_task_wait(nesting1, nesting2) << endl;
    cout << "nested-wait(" << nesting2 << ", " << nesting1  << ") = " 
        << nested_task_wait(nesting2, nesting1) << endl;

    cout << "nested     (" << nesting1 << ", " << nesting2  << ") = " 
         << nested_task_apply_count(nesting1, nesting2) << endl;
    cout << "nested     (" << nesting2 << ", " << nesting1  << ") = " 
         << nested_task_apply_count(nesting2, nesting1) << endl;

    return hpx::finalize();
}

int main(int argc, char **argv) {
    using namespace boost::assign;
    std::vector<std::string> cfg;
    cfg += "hpx.os_threads=" +        
        boost::lexical_cast<std::string>(hpx::threads::hardware_concurrency());

    boost::program_options::options_description
       desc_commandline("Usage: " HPX_APPLICATION_STRING " [options]");

    using boost::program_options::value;
    desc_commandline.add_options()
        ( "reps", value<int>()->default_value(20),
          "number of times to repeat the benchmark")
        ( "task_count", value<int>()->default_value(1024),
          "number of tasks to spawn (default 1024*num_threads")
        ( "delay_length", value<int>()->default_value(1000),
          "size of work to be done in the task") ;

    return hpx::init(desc_commandline, argc, argv, cfg);
}

/*
 * not faster than other, simpler methods
void notifying_task() {
    float a = 0.;
    for(int i = 0; i < delay_length; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
    int tasks_remaining = task_counter--;
    if(tasks_remaining == 0) {
        cond.notify_one();
    }
}
uint64_t task_cond_apply(int total_tasks) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    task_counter = 0;
    for(int i = 0; i < total_tasks; i++) {
        task_counter++;
        hpx::apply(notifying_task);
    }
    while(task_counter > 0) {
        hpx::this_thread::yield();
    }
    hpx::lcos::local::spinlock mtx;
    {
        hpx::lcos::local::spinlock::scoped_lock lk(mtx);
        while(task_counter > 0) {
            cond.wait(lk);
        }
    }
    return hpx::util::high_resolution_clock::now() - start;
}
*/
