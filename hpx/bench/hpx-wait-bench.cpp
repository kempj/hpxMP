
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
atomic<int> num_tasks{0};

void placeholder_task() {
    float a = 0.;
    for(int i = 0; i < delay_length; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
    num_tasks--;
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
    num_tasks = 0;
    for(int i = 0; i < total_tasks; i++) {
        num_tasks++;
        hpx::async(placeholder_task);
    }
    while(num_tasks > 0) {
        hpx::this_thread::yield();
    }
    return hpx::util::high_resolution_clock::now() - start;
}

//test apply
//test lock and signal

int hpx_main(boost::program_options::variables_map& vm) {
    int delay_length = vm["delay_length"].as<int>();
    int num_threads = hpx::get_os_thread_count();
    int total_tasks = num_threads * 1024;

    cout << "time for wait_all = " << task_spawn_wait(total_tasks) << endl;
    cout << "time for count    = " << task_spawn_count(total_tasks) << endl;
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
        ( "delay_length", value<int>()->default_value(1000),
          "size of work to be done in the task") ;

    return hpx::init(desc_commandline, argc, argv, cfg);
}
