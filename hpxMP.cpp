#include "hpxMP.h"
#include <iostream>
#include <cstdlib>
#include <vector>
#include <string>

#include <hpx/hpx.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/include/lcos.hpp>

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>


using namespace std;
using hpx::lcos::local::barrier;
using hpx::lcos::future;

vector<hpx::lcos::future<void>> threads;
//hpx::lcos::local::mutex single_lock;
typedef hpx::lcos::local::spinlock mutex_type;
mutex_type single_lock;

void (*omp_task)(int, void*)=0;
frame_pointer_t parent_fp = 0;
int num_threads = 0;
bool started = false;
barrier *globalBarrier;
int single_counter = 0;
int current_single_thread = -1;

struct thread_data {
    int thread_num;
    vector<future<void>> task_handles;
};

int init_num_threads() {
    int numThreads = 0;
    auto envNum = getenv("OMP_NUM_THREADS");
    if( envNum != 0)
       numThreads =  atoi(envNum);
    else 
        numThreads = hpx::threads::hardware_concurrency();
    return numThreads;
}

void thread_setup(void (*micro_task)(int, void*), int thread_num, void *fp) {
    thread_data *data_struct = new thread_data;
    data_struct->thread_num = thread_num;
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(data_struct));
    micro_task(thread_num, fp);
}

int hpx_main() {
//    cout << "OS threads = " << hpx::get_os_thread_count() << endl;
    threads.reserve(num_threads);
    globalBarrier = new barrier(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async(thread_setup, *omp_task, i, parent_fp));
    }
    hpx::lcos::wait(threads);
    return hpx::finalize();
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) {
    /*
    if(started) {
        //This shouldn't happen, because __ompc_can_fork() should prevent it any nested parallelism
        //but with tasks, I'm not sure.
        int threads_to_use = Nthreads;
        if(threads_to_use == 0)
            threads_to_use = num_threads;
        vector<future<void>> local_threads;
        local_threads.reserve(threads_to_use);
        for(int i = 0; i < threads_to_use; i++) {
            local_threads.push_back(hpx::async(*micro_task, i, fp));
        }
        hpx::lcos::wait(local_threads);
    } else {*/
    assert(!started);
    started = true;
    omp_task = micro_task;
    parent_fp = fp;
    if(Nthreads == 0)
        num_threads = init_num_threads();
    else
        num_threads = Nthreads;
    using namespace boost::assign;
    std::vector<std::string> cfg;
    cfg += "hpx.os_threads=" +
        boost::lexical_cast<std::string>(num_threads);

    char const* hpx_args_raw = getenv("OMP_HPX_ARGS");

    int argc;
    char ** argv;

    if (hpx_args_raw)
    { 
        std::vector<std::string> hpx_args;

        boost::algorithm::split(std::string(hpx_args_raw), hpx_args,
            boost::algorithm::is_any_of(";"),
                boost::algorithm::token_compress_on);

        // FIXME: For correctness check for signed overflow.
        argc = hpx_args.size();
        argv = new char*[argc + 1];
        argv[0] = "hpxMP";

        // FIXME: Should we do escaping?    
        for (boost::uint64_t i = 0; i < hpx_args.size(); ++i)
            argv[i + 1] = hpx_args[i].c_str();
    }

    else
    {
        argc = 1;
        argv = new char*[argc];
    }

//    argv[1] = "--hpx:dump-config";
//    argv[2] = "--hpx:print-bind";
    hpx::init(argc, argv, cfg);
    started = false;

    delete[] argv;
}

int __ompc_can_fork() {
    return !started;
}

int __ompc_get_local_thread_num() {
    auto thread_id = hpx::threads::get_self_id();
    auto *data = reinterpret_cast<thread_data*>(
                    hpx::threads::get_thread_data(thread_id) );
    return data->thread_num;
}

void __ompc_static_init_4( int global_tid, omp_sched_t schedtype,
                           int *p_lower, int *p_upper, 
                           int *p_stride, int incr, 
                           int chunk) {
    int thread_num = __ompc_get_local_thread_num();
    int size;
    int *tmp;
    if(*p_upper < *p_lower) {
        tmp = p_upper;
        p_upper = p_lower;
        p_lower = tmp;
    }
    size = *p_upper - *p_lower + 1;
    int chunk_size = size/num_threads;
    if(thread_num < size % num_threads) {
        *p_lower += thread_num * (chunk_size+incr);
        *p_upper = *p_lower + chunk_size ;
    } else {
        *p_lower += (size % num_threads) * (chunk_size+incr) + (thread_num - size % num_threads ) * chunk_size;
        *p_upper = *p_lower + chunk_size - incr;
    }
}

void __ompc_barrier() {
    __ompc_ebarrier();
}

void __ompc_ebarrier() {
    globalBarrier->wait();
}

int __ompc_get_num_threads(){
    return 0;
}

int __ompc_master(int global_tid){
    if(__ompc_get_local_thread_num() == 0) 
        return 1;
    return 0;
}

void __ompc_end_master(int global_tid){
}

//typedef hpx::lcos::local::spinlock my_mutex;

int __ompc_single(int global_tid){
    int tid = __ompc_get_local_thread_num();
    mutex_type::scoped_lock l(single_lock);
    //boost::lock_guard<hpx::lcos::local::mutex> l(single_lock);
    //single_lock.lock();
    if(current_single_thread == -1 && single_counter == 0) {
        current_single_thread = tid;
        single_counter = 1 - num_threads;
    } else {
        single_counter++;
    }
    //single_lock.unlock();
    if(current_single_thread == tid) 
        return 1;
    return 0;
}

void __ompc_end_single(int global_tid){
    mutex_type::scoped_lock l(single_lock);
    //boost::lock_guard<hpx::lcos::local::mutex> l(single_lock);
    //single_lock.lock();
    if(single_counter == 0) {
        current_single_thread = -1;
    }
    //single_lock.unlock();
}

int __ompc_task_will_defer(int may_delay){
    //in the OpenUH runtime, this also checks if a task limit has been reached
    //leaving that to hpx to decide
    return may_delay;
}
/*
void __ompc_task_firstprivates_alloc(void **firstprivates, int size){
    *firstprivates = aligned_malloc(size, CACHE_LINE_SIZE);
}

void __ompc_task_firstprivates_free(void *firstprivates){
    aligned_free(firstprivates);
}
*/
void task_setup(omp_task_func task_func, int thread_num, void *firstprivates, void *fp) {
    thread_data *data_struct = new thread_data;
    data_struct->thread_num = thread_num;
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(data_struct));
    task_func(firstprivates, fp);
}

void __ompc_task_create( omp_task_func taskfunc, void *frame_pointer,
                         void *firstprivates, int may_delay,
                         int is_tied, int blocks_parent) {

    auto *data = reinterpret_cast<thread_data*>(
                hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    int current_tid = data->thread_num;
    data->task_handles.push_back(hpx::async(task_setup, taskfunc, current_tid, firstprivates, frame_pointer));
//            hpx::async(taskfunc, firstprivates, frame_pointer));
}

void __ompc_task_wait(){
    auto *data = reinterpret_cast<thread_data*>(
                hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    hpx::wait(data->task_handles); 
    data->task_handles.clear();
}

void __ompc_task_exit(){
}


void __ompc_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}
void __ompc_end_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}

int omp_get_num_threads() {
    return num_threads;
}

int omp_get_max_threads() {
    return num_threads;
}

int omp_get_thread_num() {
    return __ompc_get_local_thread_num();
}

