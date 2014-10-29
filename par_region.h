#include <atomic>
#include <vector>
#include "hpx_runtime.h"


class loop_data {
    public:
        loop_data(int NT) : num_threads(NT){}
        void yield(){ hpx::this_thread::yield(); }
        void lock(){ loop_mtx.lock(); }
        void unlock(){ loop_mtx.unlock();}

        int lower;
        int upper;
        int stride;
        int chunk;
        std::atomic<int> loop_count{0}; //unused inside the loop
        std::atomic<int> num_workers{0}; 
        bool is_done = false;
        std::atomic<int> ordered_count{0};
        std::atomic<int> schedule_count{0};
        int num_threads;
        int schedule;
        std::vector<int> local_iter;
        std::vector<int> iter_remaining;
        mutex_type loop_mtx{};
};

struct parallel_region {
    parallel_region(int N) : globalBarrier(N), num_threads(N), loop_sched(N){};
    int num_threads;
    atomic<int> num_tasks{0};
    hpx::lcos::local::condition_variable thread_cond;
    barrier globalBarrier;
    mutex_type single_mtx{}; 
    mutex_type crit_mtx{};
    mutex_type thread_mtx{};
    loop_data loop_sched;

};
