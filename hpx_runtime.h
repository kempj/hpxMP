//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

//#include <hpx/hpx.hpp>
//#include <hpx/hpx_fwd.hpp>
//#include <hpx/hpx_start.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/util/static.hpp>
#include <hpx/include/lcos.hpp>

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>

#include <hpx/util/high_resolution_timer.hpp>
#include <map>


#include <hpx/include/thread_executors.hpp>

typedef void *frame_pointer_t;
typedef int omp_tid;
typedef void (*omp_micro)(int , frame_pointer_t);

typedef void (*omp_task_func)(void *firstprivates, void *fp);

typedef hpx::lcos::local::spinlock mutex_type;
typedef boost::shared_ptr<mutex_type> mtx_ptr;

using hpx::threads::executors::local_priority_queue_executor;
using hpx::lcos::local::barrier;
using hpx::lcos::shared_future;
using hpx::lcos::future;
using std::cout;
using std::endl;
using std::vector;
using std::map;
using hpx::util::high_resolution_timer;

class thread_data {
    public:
        thread_data(int tid):thread_num(tid){};
        mutex_type thread_mutex;
        int thread_num;
        vector<shared_future<void>> task_handles;
        vector<shared_future<void>> child_tasks;
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
        void lock(int lock_id);
        bool trylock(int lock_id);
        void unlock(int lock_id);
        int new_mtx();
        void create_task(omp_task_func taskfunc, void *frame_pointer,
                         void *firstprivates,// int may_delay,
                         int is_tied);//, int blocks_parent);
        void task_wait();
        double get_time();
        void delete_hpx_objects();
        omp_task_func task_func;
        frame_pointer_t fp;
        int threads_requested;
        void env_init();
        mutex_type runtime_mtx;
        vector<mtx_ptr> lock_list;
        boost::shared_ptr<high_resolution_timer> walltime;
        boost::shared_ptr<barrier> globalBarrier;
        int single_mtx_id; 
        int crit_mtx_id;
        int lock_mtx_id;
        boost::shared_ptr<local_priority_queue_executor> task_executor;
        
    private:
        int num_threads;
        int num_procs;
        std::vector<std::string> cfg;
        int argc;
        char ** argv;
};

