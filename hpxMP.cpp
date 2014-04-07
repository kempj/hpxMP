//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include "hpx_runtime.h"
#include "hpxMP.h"
#include <iostream>
#include <cstdlib>
#include <vector>
#include <string>

using namespace std;
boost::shared_ptr<hpx_runtime> hpx_backend;

bool started = false;
boost::atomic<bool> running(false);
int single_counter = 0;
int current_single_thread = -1;
int single_mtx_id = -1;
int crit_mtx_id = -1;
int lock_mtx_id = -1;
int print_mtx_id = -1;

omp_micro thread_func = 0;

int init_num_threads() {
    int numThreads = 0;
    auto envNum = getenv("OMP_NUM_THREADS");
    if( envNum != 0)
       numThreads = atoi(envNum);
    return numThreads;
}

void omp_thread_func(void *firstprivates, void *fp) {
    //Threads do not need first privates. 
    //This function allows a thread to be handled the same way a task is.
    int tid = __ompc_get_local_thread_num();
    thread_func(tid, fp);
    //hpx_backend->thread_wait();
}

//overwrites global in openmp
int __ompc_init_rtl(int num_threads) {
    return 0;
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) {
    if(!running) {
        running = true;
//        hpx_backend.init(Nthreads);
        hpx_backend.reset(new hpx_runtime(Nthreads));       
    }
    if(print_mtx_id == -1) 
        print_mtx_id = hpx_backend->new_mtx();
    if(single_mtx_id == -1) 
        single_mtx_id = hpx_backend->new_mtx();
    if(crit_mtx_id == -1)
        crit_mtx_id = hpx_backend->new_mtx();
    if(lock_mtx_id == -1)
        lock_mtx_id = hpx_backend->new_mtx();
    if(Nthreads <= 0)
        Nthreads = hpx_backend->get_num_threads();
    thread_func = micro_task;
    assert(!started);
    started = true;
    hpx_backend->fork(Nthreads, omp_thread_func, fp);
    started = false;
}

int __ompc_can_fork() {
    return !started;
}
int __ompc_get_local_thread_num() {
    //TODO: what if backend has not started yet?
    if(!running) {
        running = true;
        hpx_backend.reset(new hpx_runtime(0));
    }
    if(!started) {
        return 0;
    } else {
        return hpx_backend->get_thread_num();
    }
}

//ignoring chunk size input, assuming it is one
void __ompc_static_init_4( int global_tid, omp_sched_t schedtype,
                           int *p_lower, int *p_upper, 
                           int *p_stride, int incr, 
                           int chunk) {
//    hpx_backend->lock(print_mtx_id);
//    cout << "Thread " << thread_num << " of " << num_threads <<  endl
//         << "\t" << *p_lower << "-" << *p_upper << ", " << *p_stride << endl;

    //copied very directly from the openUH OpenMP runtime:
    int block_size, stride, my_lower, my_upper;
    int team_size = omp_get_num_threads();
    if (schedtype == OMP_SCHED_STATIC_EVEN) {
        int trip_count = (*p_upper - *p_lower) / incr + 1;
        int adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
        stride = (trip_count / team_size + adjustment + 1) * incr;
        block_size = (trip_count / team_size + adjustment) * incr;
        my_lower = *p_lower + global_tid * stride;
        my_upper = my_lower + block_size;
    } else { /* OMP_SCHED_STATIC*/
        block_size = (chunk - 1) * incr;
        stride = chunk * incr;
        my_lower = *p_lower + global_tid * stride;
        my_upper = my_lower + block_size;
        *p_stride = stride * team_size;
    }
    *p_lower = my_lower;
    *p_upper = my_upper;

 //   cout << "\t" << *p_lower << "-" << *p_upper << ", " << *p_stride << endl;
 //   hpx_backend->unlock(print_mtx_id);
}

void __ompc_static_init_8( omp_int32 global_tid, omp_sched_t schedtype,
                      omp_int64 *p_lower, omp_int64 *p_upper, omp_int64 *p_stride,
                      omp_int64 incr, omp_int64 chunk ){
    omp_int64 block_size, stride, my_lower, my_upper;
    int team_size = omp_get_num_threads();
    if (schedtype == OMP_SCHED_STATIC_EVEN) {
        omp_int64 trip_count = (*p_upper - *p_lower) / incr + 1;
        omp_int64 adjustment = ((trip_count % team_size) == 0) ? -1 : 0;
        stride = (trip_count / team_size + adjustment + 1) * incr;
        block_size = (trip_count / team_size + adjustment) * incr;
        my_lower = *p_lower + global_tid * stride;
        my_upper = my_lower + block_size;
    } else { /* OMP_SCHED_STATIC*/
        block_size = (chunk - 1) * incr;
        stride = chunk * incr;
        my_lower = *p_lower + global_tid * stride;
        my_upper = my_lower + block_size;
        *p_stride = stride * team_size;
    }
    *p_lower = my_lower;
    *p_upper = my_upper;
}

void __ompc_scheduler_init_4( omp_int32 global_tid,
                              omp_sched_t schedtype,
                              omp_int32 lower, omp_int32 upper,
                              omp_int32 stride, omp_int32 chunk){
    cout << "Not implemented: __ompc_scheduler_init_4" << endl;
}

void __ompc_scheduler_init_8( omp_int32 global_tid,
                              omp_sched_t schedtype,
                              omp_int64 lower, omp_int64 upper,
                              omp_int64 stride, omp_int64 chunk){
    cout << "Not implemented: __ompc_scheduler_init_8" << endl;
}

omp_int32 __ompc_schedule_next_4( omp_int32 global_tid,
                                  omp_int32 *plower, omp_int32 *pupper,
                                  omp_int32 *pstride){
    cout << "Not implemented: __ompc_schedule_next_4" << endl;
    return 0;
}

omp_int32 __ompc_schedule_next_8( omp_int32 global_tid,
                                  omp_int64 *plower, omp_int64 *pupper,
                                  omp_int64 *pstride){
    cout << "Not implemented: __ompc_schedule_next_8" << endl;
    return 0;
}

void __ompc_reduction(omp_int32 gtid, omp_int32 **lck){
    __ompc_critical(gtid, lck);
}

void __ompc_end_reduction(omp_int32 gtid, omp_int32 **lck){
    __ompc_end_critical(gtid, lck);
}

void __ompc_barrier() {
    __ompc_ebarrier();
}

void __ompc_ebarrier() {
    //This is added because a barrier is supposed to wait for all current tasks to finish.
    //In the case where tasks were spawned, but taskwait was not called, this is needed
    hpx_backend->task_wait();

    hpx_backend->barrier_wait();
}

int __ompc_get_num_threads(){
    return hpx_backend->get_num_threads();
}

int __ompc_master(int global_tid){
    if(__ompc_get_local_thread_num() == 0) 
        return 1;
    return 0;
}

void __ompc_end_master(int global_tid){
}

int __ompc_single(int tid){
    hpx_backend->lock(single_mtx_id);
    int num_threads = __ompc_get_num_threads();
    if(current_single_thread == -1 && single_counter == 0) {
        current_single_thread = tid;
        single_counter = 1 - num_threads;
    } else {
        single_counter++;
    }
    hpx_backend->unlock(single_mtx_id);
    if(current_single_thread == tid) {
        return 1;
    }
    return 0;
}

void __ompc_end_single(int tid){
    hpx_backend->lock(single_mtx_id);
    if(single_counter == 0) {
        current_single_thread = -1;
    }
    hpx_backend->unlock(single_mtx_id);
}

int __ompc_task_will_defer(int may_delay){
    //in the OpenUH runtime, this also checks if a task limit has been reached
    //leaving that to hpx to decide
    //Not sure if this is correct
    return may_delay;
}
void __ompc_task_firstprivates_alloc(void **firstprivates, int size){
    *firstprivates = malloc(size);
}

void __ompc_task_firstprivates_free(void *firstprivates){
    free(firstprivates);
}

void __ompc_task_create( omp_task_func task_func, void *frame_pointer,
                         void *firstprivates, int may_delay,
                         int is_tied, int blocks_parent) {
    // blocks_parent keeps parent tasks from exiting when they have shared 
    // variables in nested tasks. Since the current hpxMP implementation
    // calls a taskwait at the end of each task, the blocks_parent variable
    // is not used.

    // TODO: Optimization, store is_tied in the thread/task, and whenever 
    // waiting on it, check it, or possibly have a separate vector to store
    // untied tasks
    if(may_delay == 0) {
       task_func(firstprivates, frame_pointer);
    } else {
        hpx_backend->create_task( task_func, frame_pointer, firstprivates,
                                  is_tied);
    }
}

void __ompc_task_wait(){
    hpx_backend->task_wait();
}

void __ompc_task_exit(){
    //The main 'thread' tasks need to wait for all chilren tasks to finish,
    // even if the child tasks don't wait on their child tasks to finish.
    // This is a simple solution that forces all tasks to wait on child tasks 
    // to finish. This is not incorrect, but it could hurt performance.

    // If the information of whether or not the current thread was a 'task'
    // or a 'thread' was stored, this could be avoided
    //
    // moved into the actual thread function
}

void __ompc_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}
void __ompc_end_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}

void __ompc_critical(int gtid, int **lck) {
    if(*lck == NULL || **lck < 0) {
        hpx_backend->lock(crit_mtx_id);
        if(*lck == NULL || **lck < 0){
            *lck = new int;
            **lck = hpx_backend->new_mtx();
        }
        hpx_backend->unlock(crit_mtx_id);
    }
    hpx_backend->lock(**lck);
}

void __ompc_end_critical(omp_int32 gtid, omp_int32 **lck) {
    hpx_backend->unlock(**lck);
}

omp_int32 __ompc_get_thdprv( void *** thdprv_p, omp_int64 size, 
                             void *datap, omp_int32 global_tid) {
    cout << "unimplemented function called: __ompc_get_thdprv" << endl;
    return 1;
}
omp_int32 __ompc_copyin_thdprv(int num,...) {
    cout << "unimplemented function called: __ompc_copyin_thdprv" << endl;
    return 0;
}

omp_int32 __ompc_copyprivate( omp_int32 mpsp_status,
                              void *cppriv, 
                              void(*cp)(void* src, void* dst) ) {
    cout << "unimplemented function called: __ompc_copyprivate" << endl;
    return 0;
}
//OMP Library functions
//TODO: move to another file
int omp_get_num_threads() {
    return hpx_backend->get_num_threads();
}

int omp_get_max_threads() {
    //TODO: what if backend has not started yet?
    if(!running) {
        running = true;
        hpx_backend.reset(new hpx_runtime(0));
    }                      
    return hpx_backend->get_num_threads();
}

int omp_get_thread_num() {
    return __ompc_get_local_thread_num();
}

double omp_get_wtime() {
    return hpx_backend->get_time();
}
double omp_get_wtick() {
    //high resolution elapsed_min
    return .000000001;
}

void omp_init_lock(volatile omp_lock_t *lock) {
    hpx_backend->lock(lock_mtx_id);
    int new_id = hpx_backend->new_mtx();
    hpx_backend->unlock(lock_mtx_id);

    *lock = reinterpret_cast<omp_lock_t>(new_id);
}

void omp_destroy_lock(volatile omp_lock_t *lock) {
}

void omp_set_lock(volatile omp_lock_t *lock) {
    int lock_id = *((int*)lock);
    hpx_backend->lock(lock_id);
}

void omp_unset_lock(volatile omp_lock_t *lock) {
    int lock_id = *((int*)lock);
    hpx_backend->unlock(lock_id);
}

int omp_test_lock(volatile omp_lock_t *lock) {
    if(hpx_backend->trylock(*((int*)lock)))
        return 1;
    return 0;
}

void omp_init_nest_lock(volatile omp_nest_lock_t *lock) {
    //unimplmented
}

void omp_destroy_nest_lock(volatile omp_nest_lock_t *lock) {
    //unimplmented
}

void omp_set_nest_lock(volatile omp_nest_lock_t *lock) {
    //unimplmented
}

void omp_unset_nest_lock(volatile omp_nest_lock_t *lock) {
    //unimplmented
}

int omp_test_nest_lock(volatile omp_nest_lock_t *lock) {
    //unimplmented
    return 1;
}

void omp_set_nested() {
    //unimplmented
}

int omp_in_parallel(void){
    return started;
}

void omp_set_dynamic(int dynamic_threads){
    //The omp_set_dynamic routine enables or disables dynamic adjustment of the
    //number of threads available for the execution of subsequent parallel regions by
    //setting the value of the dyn-var ICV.
}
