
#include <hpx/hpx_init.hpp>
#include <hpx/hpx.hpp>
//#include <hpx/parallel/executors/thread_pool_executors.hpp>

#include <hpx/runtime/threads/executors/thread_pool_executors.hpp>
//#include <hpx/hpx_fwd.hpp>
//#include <hpx/runtime/threads/thread_executor.hpp>
//#include <hpx/exception.hpp>

#include <cstdlib>
#include <vector>

using std::cout;
using std::endl;

typedef hpx::threads::executors::static_priority_queue_executor static_executor;

struct implicit_task_executor : public static_executor
{
    implicit_task_executor(std::size_t max_punits, std::size_t min_punits = 1)
        : static_executor(max_punits, min_punits)
    {}

     void add(closure_type && f, char const* description,
             hpx::threads::thread_state_enum initial_state, bool run_now,
             hpx::threads::thread_stacksize stacksize, hpx::error_code& ec,
             std::size_t os_thread) {
     }

};

void f1 () { 
    std::size_t tid = hpx::get_worker_thread_num();
    cout << "test function #" << tid << endl; 
} 

int hpx_main(int argc, char* argv[])
{
    std::size_t num_threads = hpx::get_os_thread_count();
    cout << "num_threads = " << num_threads << endl;

    {
        implicit_task_executor exec(num_threads, num_threads);
        hpx::error_code ec = hpx::throws;
        
        for( int i = 0; i < num_threads * 2; i++) {
            exec.add(std::bind(f1), "", hpx::threads::pending, true, 
                    hpx::threads::thread_stacksize_default, ec, i);
        }
    }

    return hpx::finalize();
}

int main(int argc, char* argv[])
{
    std::vector<std::string> cfg;
    cfg.push_back("hpx.os_threads=" +
        boost::lexical_cast<std::string>(hpx::threads::hardware_concurrency()));

    hpx::init(argc, argv, cfg);

    return 0;
}
