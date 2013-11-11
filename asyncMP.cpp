#include "hpxMP.h"
#include <iostream>
#include <cstdlib>

#include <hpxc/threads.h>
#include <hpx/hpx.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/include/lcos.hpp>

using namespace std;
using hpx::lcos::local::barrier;
using hpx::lcos::future;

//hpxc_thread_t *threads;
vector<hpx::lcos::future<void>> threads;
hpxc_mutex_t single_lock = HPXC_MUTEX_INITIALIZER;

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

//There is a reference to a struct being passed as the second argument to the 
//micro_taskthat seems to be accessed only on nested parallel regions. 
//The first argument is the global_tid.
//So, nested parallel regions should be disabled now; running in serial after the first fork
int hpx_main() {
    threads.reserve(num_threads);
    globalBarrier = new barrier(num_threads);
//    cout << "hello from hpx main (" << num_threads << " threads)" << endl;
    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async(*omp_task, 0, (void*)0));
//        hpxc_thread_create( &threads[i], 0, (void* (*)(void*))omp_task, 0);
    }
    hpx::lcos::wait(threads);
    return hpx::finalize();
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) {
    if(started) {
        vector<future<void>> local_threads;
        local_threads.reserve(num_threads);
        for(int i = 0; i < num_threads; i++) {
            local_threads.push_back(hpx::async(*micro_task, 0, fp));
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
    /*
    for(int i = 0; i < num_threads; i++) {
        if(hpxc_thread_equal(hpxc_thread_self(), threads[i])) {
        hpx::threads::thread_self* self = hpx::threads::get_self_ptr();
        if(*self ==  threads[i]) {
            return i;
        }
    }*/
    return 0;
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
    hpxc_mutex_lock(&single_lock);
    if(current_single_thread == -1 && single_counter == 0) {
        current_single_thread = tid;
        single_counter = 1 - num_threads;
    } else {
        single_counter++;
    }
    hpxc_mutex_unlock(&single_lock);
    if(current_single_thread == tid) 
        return 1;
    return 0;
}

void __ompc_end_single(int global_tid){
    hpxc_mutex_lock(&single_lock);
    if(single_counter == 0) {
        current_single_thread = -1;
    }
    hpxc_mutex_unlock(&single_lock);
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

