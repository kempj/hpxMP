
#include <hpx/hpx_init.hpp>
#include <hpx/hpx.hpp>
#include <hpx/parallel/executors/thread_pool_executors.hpp>

#include <hpx/runtime/threads/executors/thread_pool_executors.hpp>

#include <cstdlib>
#include <vector>

using std::cout;
using std::endl;

typedef hpx::parallel::static_priority_queue_executor static_executor;

struct implicit_task_executor : public static_executor
{
    void async_execute(int id) {
        //exec_.add();
        
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
        static_executor exec(num_threads, num_threads);
        //typedef hpx::parallel::executor_traits<static_executor> traits;
        
        cout << "Num threads in the executor: " << exec.os_thread_count() << endl;
        for( int i = 0; i < num_threads * 2; i++) {
            exec.async_execute(f1);
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
