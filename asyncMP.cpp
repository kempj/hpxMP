#include "hpxMP.h"
#include <iostream>
#include <cstdlib>

#include <hpx/hpx.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/include/lcos.hpp>

using namespace std;
using hpx::lcos::local::barrier;
using hpx::lcos::future;

vector<hpx::lcos::future<void>> threads;
hpx::lcos::local::spinlock single_lock;


void (*omp_task)(int, void*)=0;
int num_threads = 0;
bool started = false;
barrier *globalBarrier;
int single_counter = 0;
int current_single_thread = -1;


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
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, thread_num);
    micro_task(thread_num, fp);
}

int hpx_main() {
    threads.reserve(num_threads);
    globalBarrier = new barrier(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async(thread_setup, *omp_task, i, (void*)0));
    }
    hpx::lcos::wait(threads);
    return hpx::finalize();
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) {
    if(started) {
        vector<future<void>> local_threads;
        local_threads.reserve(num_threads);
        for(int i = 0; i < num_threads; i++) {
            local_threads.push_back(hpx::async(*micro_task, i, fp));//should this be i or the current omp_thread_num
        }
        hpx::lcos::wait(local_threads);
    } else {
        started = true;
        omp_task = micro_task;
        num_threads = init_num_threads();
        hpx::init();
        started = false;
    }
}

int __ompc_can_fork() {
    return !started;
}

int __ompc_get_local_thread_num() {
    auto thread_id = hpx::threads::get_self_id();
    int thread_num = hpx::threads::get_thread_data( thread_id );
    return thread_num;
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

int __ompc_single(int global_tid){
    int tid = __ompc_get_local_thread_num();
    single_lock.lock();
    if(current_single_thread == -1 && single_counter == 0) {
        current_single_thread = tid;
        single_counter = 1 - num_threads;
    } else {
        single_counter++;
    }
    single_lock.unlock();
    if(current_single_thread == tid) 
        return 1;
    return 0;
}

void __ompc_end_single(int global_tid){
    single_lock.lock();
    if(single_counter == 0) {
        current_single_thread = -1;
    }
    single_lock.unlock();
}

void __ompc_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}
void __ompc_end_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}
void __ompc_task_exit() {
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

