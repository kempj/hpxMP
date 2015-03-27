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

#include "icv-vars.h"


typedef void (*microtask_t)( int *gtid, int *npr, ... );
typedef int (*invoke_func)( microtask_t , int , int , int , void**);

typedef void *frame_pointer_t;
typedef int omp_tid;
typedef void (*omp_micro)(int , frame_pointer_t);

typedef void (*omp_task_func)(void *firstprivates, void *fp);

typedef hpx::lcos::local::spinlock mutex_type;
typedef boost::shared_ptr<mutex_type> mtx_ptr;

typedef int (* kmp_routine_entry_t)( int, void * );

typedef std::map<int64_t, hpx::shared_future<void>> depends_map;

typedef struct kmp_task {
    void *              shareds;
    kmp_routine_entry_t routine;
    int                 part_id;
#if OMP_40_ENABLED
    kmp_routine_entry_t destructors;
#endif
} kmp_task_t;


using std::atomic;
using boost::shared_ptr;
using hpx::threads::executors::local_priority_queue_executor;
using hpx::lcos::local::barrier;
using hpx::lcos::shared_future;
using hpx::lcos::future;
using std::cout;
using std::endl;
using std::vector;
//using std::map;
using hpx::util::high_resolution_timer;
using hpx::threads::set_thread_data;
using hpx::threads::get_thread_data;
using hpx::threads::get_self_id;
using hpx::lcos::local::dataflow;
using hpx::util::unwrapped;
using hpx::make_ready_future;


class loop_data {
    //TODO: does this need to be changed to work with teams?
    public:
        loop_data(int NT) : num_threads(NT), first_iter(NT,0), last_iter(NT,0), iter_count(NT,0){}
        void yield(){ hpx::this_thread::yield(); }
        void lock(){ loop_mtx.lock(); }
        void unlock(){ loop_mtx.unlock();}

        std::atomic<int> lower{0};
        int upper;
        int stride;
        int chunk;
        std::atomic<int> loop_count{0}; //unused inside the loop
        std::atomic<int> num_workers{0}; 
        bool work_remains = false;
        std::atomic<int> ordered_count{0};
        std::atomic<int> schedule_count{0};
        int num_threads;
        int schedule;
        int total_iter;
        std::vector<int> first_iter;
        std::vector<int> last_iter;
        std::vector<int> iter_count;
        int iter_size;
        mutex_type loop_mtx{};
        bool ordered = false;//do I need this?
};

//Does this need to keep track of the parallel region it is nested in,
// the omp_task_data of the parent thread, or both?
struct parallel_region {

    parallel_region( int N ) : num_threads(N), globalBarrier(N), 
                               loop_sched(N), depth(0),
                               reduce_data(N, 0) {};

    parallel_region( parallel_region *parent, int threads_requested ) : parallel_region(threads_requested) {
        depth = parent->depth + 1; 
    }
    int num_threads;
    hpx::lcos::local::condition_variable cond;
    barrier globalBarrier;
    mutex_type single_mtx{}; 
    mutex_type crit_mtx{};
    mutex_type thread_mtx{};
    loop_data loop_sched;
    int depth;
    atomic<int> single_counter{0};
    atomic<int> current_single_thread{-1};
    void *copyprivate_data;
    vector<void*> reduce_data;
    atomic<int> num_tasks{0};
};


//What parts of a task could I move to a shared state to get a performance
// improvement, or some other, orgizational improvement?
// icvs?
class omp_task_data {
    public:
        //This constructor should only be used once for the implicit task
        omp_task_data( parallel_region *T, omp_device_icv *global, int init_num_threads) 
            : team(T), num_child_tasks(new atomic<int>{0}), 
              num_thread_tasks(new atomic<int>{0}) {
            thread_num = 0;
            icv.device = global;
            icv.nthreads = init_num_threads;
            threads_requested = icv.nthreads;
        };

        //should be used for implicit tasks/threads
        omp_task_data(int tid, parallel_region *T, omp_task_data *P )
            : omp_task_data(tid, T, P->icv)
        {
            num_thread_tasks.reset(new atomic<int>{0});
            icv.levels++;
            if(team->num_threads > 1) {
                icv.active_levels++;
            }
        };

        //This is for explicit tasks
        omp_task_data(int tid, parallel_region *T, omp_icv icv_vars)
            : thread_num(tid), team(T), icv(icv_vars), num_child_tasks(new atomic<int>{0}) 
        {
            threads_requested = icv.nthreads;
            icv_vars.device = icv.device;
        };

        //assuming the number of threads that can be created is infinte (so I can avoid using ThreadsBusy)
        //See section 2.3 of the OpenMP 4.0 spec for details on ICVs.
        void set_threads_requested( int nthreads ){
            if( nthreads > 0) {
                threads_requested = nthreads;
            }
            int active_regions = icv.active_levels;
            if( icv.nest && active_regions > 1) {
                threads_requested = 1;
            }
            if(active_regions == icv.device->max_active_levels) {
                threads_requested = 1;
            }
        }
        
        int thread_num;
        int threads_requested;
        parallel_region *team;
        mutex_type thread_mutex;
        hpx::lcos::local::condition_variable thread_cond;
        shared_ptr<atomic<int>> num_taskgroup_tasks;
        shared_ptr<atomic<int>> num_thread_tasks;
        shared_ptr<atomic<int>> num_child_tasks;

        omp_icv icv;
        depends_map df_map;
};

class hpx_runtime {
    public:
        hpx_runtime();
        void fork(invoke_func kmp_invoke, microtask_t thread_func, int argc, void** argv);
        parallel_region* get_team();
        omp_task_data* get_task_data();
        int get_thread_num();
        int get_num_threads();
        int get_num_procs();
        void set_num_threads(int nthreads);
        void barrier_wait();
        void create_task( omp_task_func taskfunc, void *frame_pointer,
                          void *firstprivates, int is_tied, int blocks_parent);
        void create_task( kmp_routine_entry_t taskfunc, int gtid, kmp_task_t *task);
        void create_df_task( int gtid, kmp_task_t *task, vector<int64_t> in, vector<int64_t> out);
        void task_exit();
        void task_wait();
        double get_time();
        void delete_hpx_objects();
        void env_init();
        bool nesting_enabled();
        void** get_threadprivate();

    private:
        shared_ptr<parallel_region> implicit_region;
        shared_ptr<omp_task_data> initial_thread;
        int num_procs;
        shared_ptr<high_resolution_timer> walltime;
        bool external_hpx;
        omp_device_icv device_icv;

        //atomic<int> threads_running{0};//ThreadsBusy
};

