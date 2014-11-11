//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <limits>

#include <hpx/hpx.hpp>
#include <hpx/hpx_start.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/util/static.hpp>
#include <hpx/include/lcos.hpp>
#include <hpx/lcos/local/condition_variable.hpp>


#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <atomic>

#include <hpx/util/high_resolution_timer.hpp>
#include <map>


typedef void *frame_pointer_t;
typedef int omp_tid;
typedef void (*omp_micro)(int , frame_pointer_t);

typedef void (*omp_task_func)(void *firstprivates, void *fp);

typedef hpx::lcos::local::spinlock mutex_type;
typedef boost::shared_ptr<mutex_type> mtx_ptr;

using std::atomic;
using boost::shared_ptr;
using hpx::threads::executors::local_priority_queue_executor;
using hpx::lcos::local::barrier;
using hpx::lcos::shared_future;
using hpx::lcos::future;
using std::cout;
using std::endl;
using std::vector;
using std::map;
using hpx::util::high_resolution_timer;
using hpx::threads::set_thread_data;
using hpx::threads::get_thread_data;
using hpx::threads::get_self_id;


class loop_data {
    //TODO: does this need to be changed to work with teams?
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

//Does this need to keep track of the parallel region it is nested in,
// the omp_data of the parent thread, or both?
struct parallel_region {
    parallel_region(int N) : nthreads_var(N), globalBarrier(N), 
                             loop_sched(N), depth(0) {};

    parallel_region(parallel_region *parent, int threads_requested) : 
                                    parallel_region(threads_requested) {
        depth = parent->depth + 1; 
        dyn_var = parent->dyn_var;
        nest_var = parent->nest_var;
        max_active_levels = parent->max_active_levels;
    }
    int request_threads(int nthreads){
        if(nthreads <= 0)
            nthreads = nthreads_var;

        if(nest_var == false || depth > max_active_levels)
            return 1;
        //if(nthreads > hpx_backend->thread_limit_var)
        //    return 1;
        //TODO: increment ThreadsBusy, and decrement it... somewhere
        return nthreads;
    }

    int nthreads_var;
    atomic<int> num_tasks{0};
    hpx::lcos::local::condition_variable cond;
    barrier globalBarrier;
    mutex_type single_mtx{}; 
    mutex_type crit_mtx{};//TODO: this needs to be removed and the mtx in the runtime used.
    mutex_type thread_mtx{};
    loop_data loop_sched;
    int depth;
    bool dyn_var{false};//not used
    bool nest_var{false};
    int max_active_levels{std::numeric_limits<int>::max()};
    atomic<int> single_counter{0};
};

class omp_data {
    public:
        omp_data(int tid, parallel_region *T):thread_num(tid), team(T){};
        omp_data(omp_data *p): thread_num(p->thread_num),
                                parent(p), team(p->team) {};
        int thread_num;
        omp_data *parent;
        //TODO: If I keep track of task depth, can I implement a useful cutoff?
        mutex_type thread_mutex;
        hpx::lcos::local::condition_variable thread_cond;
        atomic<int> blocking_children {0};
        atomic<bool> is_finished {false};
        atomic<bool> has_dependents {false};
        vector<future<void>> task_handles;
        parallel_region *team;
        int active_levels;
        void *threadprivate{NULL};
};

class hpx_runtime {
    public:
        hpx_runtime();
        void fork(int num_threads, omp_micro thread_func, frame_pointer_t fp);
        parallel_region* get_team();
        int get_thread_num();
        int get_num_threads();
        int get_num_procs();
        void set_num_threads(int nthreads);
        void barrier_wait();
        void create_task(omp_task_func taskfunc, void *frame_pointer,
                         void *firstprivates,// int may_delay,
                         int is_tied, int blocks_parent);
        void task_exit();
        void task_wait();
        double get_time();
        void delete_hpx_objects();
        void env_init();
        bool nesting_enabled();
        void set_default_nesting(bool nest) {
            initial_nest_var = nest;
        }
        void** get_threadprivate();
        
        mutex_type crit_mtx{};
    private:
        int num_procs;
        shared_ptr<high_resolution_timer> walltime;
        bool external_hpx;

        //atomic<int> threads_running{0};//ThreadsBusy
        //ICVs:
        bool initial_nest_var{false};
        int initial_num_threads;
        //int thread_limit_var{std::numeric_limits<int>::max()};
        //int initial_max_active_levels{std::numeric_limits<int>::max()};
        //bool cancel_var{false};//not implemented
        //int stacksize_var; //-Ihpx.stacks.small_size=... (use hex numbers)
            //http://stellar-group.github.io/hpx/docs/html/hpx/manual/init/configuration/config_defaults.html
        //wait-policy-var OMP_WAIT_POLICY//This is a compile time HPX option
};

