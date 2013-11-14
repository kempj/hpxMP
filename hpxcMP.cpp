#include "hpxMP.h"
#include <iostream>
#include <cstdlib>
#include <hpxc/threads.h>

#include <hpx/hpx.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>

#include <atomic>
 
using namespace std;
using hpx::lcos::local::barrier;

hpxc_thread_t *threads;
hpxc_mutex_t single_lock = HPXC_MUTEX_INITIALIZER;

void (*omp_task)(int, void*)=0;
int num_threads = 0;
bool started = false;
barrier *b;
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
    threads = new hpxc_thread_t[num_threads];
    b = new barrier(num_threads);
//    cout << "hello from hpx main (" << num_threads << " threads)" << endl;
    for(int i = 0; i < num_threads; i++) {
        hpxc_thread_create( &threads[i], 0, (void* (*)(void*))omp_task, 0);
    }
    for(int i = 0; i < num_threads; i++) {
        hpxc_thread_join( threads[i], 0);
    }
    return hpx::finalize();
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) {
    //struct timespec start_time, end_time;
    if(started) {
        hpxc_thread_t *local_threads = new hpxc_thread_t[num_threads];
        for(int i = 0; i < num_threads; i++) {
            hpxc_thread_create( &local_threads[i], 0, (void* (*)(void*))micro_task, 0);
        }
        for(int i = 0; i < num_threads; i++) {
            hpxc_thread_join(local_threads[i], 0);
        }
    } else {
        started = true;
        omp_task = micro_task;
        num_threads = init_num_threads();
        //clock_gettime(CLOCK_REALTIME, &start_time);
        hpx::init();
        //clock_gettime(CLOCK_REALTIME, &end_time);
        //cout << endl << end_time.tv_sec - start_time.tv_sec << ", "
        //     << end_time.tv_nsec - start_time.tv_nsec << endl;
        started = false;
    }
}

omp_int32 __ompc_can_fork() {
    return !started;
}

omp_int32 __ompc_get_local_thread_num() {
    for(int i = 0; i < num_threads; i++) {
        if(hpxc_thread_equal(hpxc_thread_self(), threads[i])) {
            return i;
        }
    }
    return 0;
}

void __ompc_static_init_4( omp_int32 global_tid, omp_sched_t schedtype,
                           omp_int32 *p_lower, omp_int32 *p_upper, 
                           omp_int32 *p_stride, omp_int32 incr, 
                           omp_int32 chunk) {
    int thread_num = __ompc_get_local_thread_num();
    int size;
    omp_int32 *tmp;
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
    b->wait();
}

omp_int32 __ompc_get_num_threads(){
    return 0;
}

omp_int32 __ompc_master(omp_int32 global_tid){
    if(__ompc_get_local_thread_num() == 0) 
        return 1;
    return 0;
}

void __ompc_end_master(omp_int32 global_tid){
}

omp_int32 __ompc_single(omp_int32 global_tid){
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

void __ompc_end_single(omp_int32 global_tid){
    hpxc_mutex_lock(&single_lock);
    if(single_counter == 0) {
        current_single_thread = -1;
    }
    hpxc_mutex_unlock(&single_lock);
}

int __ompc_task_will_defer(int may_delay){
    //in the OpenUH runtime, this also checks if a task limit has been reached
    //leaving that to hpx to decide
    return may_delay;
}

void __ompc_task_firstprivates_alloc(void **firstprivates, int size){
    //Not sure if anything needs to be allocated yet.
}

void __ompc_task_firstprivates_free(void *firstprivates){
    //similar to alloc, not implementing yet
}

void __ompc_task_create( omp_task_func taskfunc, void *frame_pointer,
                         void *firstprivates, int may_delay,
                         int is_tied, int blocks_parent){
}

void __ompc_task_wait(){
}

void __ompc_task_exit(){
}


void __ompc_serialized_parallel(omp_int32 global_tid) {
    //It appears this function does nothing
}
void __ompc_end_serialized_parallel(omp_int32 global_tid) {
    //It appears this function does nothing
}
void __ompc_task_exit() {
    //It appears this function does nothing
}

omp_int32 omp_get_num_threads() {
    return num_threads;
}

omp_int32 omp_get_max_threads() {
    return num_threads;
}

omp_int32 omp_get_thread_num() {
    return __ompc_get_local_thread_num();
}

