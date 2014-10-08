//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

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

class omp_data {
    public:
        omp_data(int tid):thread_num(tid){};
        omp_data(omp_data *p): thread_num(p->thread_num),
                                          parent(p){};
        int thread_num;
        omp_data *parent;
        //TODO: If I keep track of 'depth' can I implement a useful cutoff?
        mutex_type thread_mutex;
        hpx::lcos::local::condition_variable thread_cond;
        atomic<int> blocking_children {0};
        atomic<bool> is_finished {false};
        atomic<bool> has_dependents {false};
        vector<future<void>> task_handles;
};

class hpx_runtime {
    public:
        hpx_runtime();
        void fork(int num_threads, omp_task_func task_func, frame_pointer_t fp);
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
        int threads_requested;
        void env_init();
        
    private:
        //Need to clarify max num_threads, num_threads and requested, and sort it with the spec
        int num_threads;
        int num_procs;
        mutex_type runtime_mtx;
        shared_ptr<high_resolution_timer> walltime;
        shared_ptr<barrier> globalBarrier;
        bool external_hpx;
};

