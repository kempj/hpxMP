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
#include <memory>

using namespace std;
unique_ptr<hpx_runtime> hpx_backend;

bool started = false;
int single_counter = 0;
int current_single_thread = -1;
int single_mtx_id = -1;

omp_micro thread_func = 0;

int init_num_threads() {
    int numThreads = 0;
    auto envNum = getenv("OMP_NUM_THREADS");
    if( envNum != 0)
       numThreads = atoi(envNum);
    return numThreads;
}

void omp_thread_func(void *firstprivates, void *fp) {
    int tid = __ompc_get_local_thread_num();
    thread_func(tid, fp);
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) {
    if(!hpx_backend)
        hpx_backend = unique_ptr<hpx_runtime>(new hpx_runtime(Nthreads));
    if(single_mtx_id == -1) 
        single_mtx_id = hpx_backend->new_mtx();
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
    return hpx_backend->get_thread_num();
}

void __ompc_static_init_4( int global_tid, omp_sched_t schedtype,
                           int *p_lower, int *p_upper, 
                           int *p_stride, int incr, 
                           int chunk) {
    int thread_num = __ompc_get_local_thread_num();
    int size;
    int *tmp;
    int num_threads = __ompc_get_num_threads();
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

void __ompc_static_init_8( omp_int32 global_tid, omp_sched_t schedtype,
                      omp_int64 *p_lower, omp_int64 *p_upper, omp_int64 *p_stride,
                      omp_int64 incr, omp_int64 chunk ){
    omp_int64 thread_num = __ompc_get_local_thread_num();
    omp_int64 size;
    omp_int64 *tmp;
    int num_threads = __ompc_get_num_threads();
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

void __ompc_scheduler_init_4( omp_int32 global_tid,
                              omp_sched_t schedtype,
                              omp_int32 lower, omp_int32 upper,
                              omp_int32 stride, omp_int32 chunk){
    //cout << "Not implemented" << endl;
}

void __ompc_scheduler_init_8( omp_int32 global_tid,
                              omp_sched_t schedtype,
                              omp_int64 lower, omp_int64 upper,
                              omp_int64 stride, omp_int64 chunk){
    //cout << "Not implemented" << endl;
}

omp_int32 __ompc_schedule_next_4( omp_int32 global_tid,
                                  omp_int32 *plower, omp_int32 *pupper,
                                  omp_int32 *pstride){
    //cout << "Not implemented" << endl;
    return 0;
}

omp_int32 __ompc_schedule_next_8( omp_int32 global_tid,
                                  omp_int64 *plower, omp_int64 *pupper,
                                  omp_int64 *pstride){
    //cout << "Not implemented" << endl;
    return 0;
}

void __ompc_reduction(omp_int32 gtid, omp_int32 **lck){
}

void __ompc_end_reduction(omp_int32 gtid, omp_int32 **lck){
}

void __ompc_barrier() {
    __ompc_ebarrier();
}

void __ompc_ebarrier() {
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

    hpx_backend->create_task( task_func, frame_pointer, firstprivates, 
                             may_delay, is_tied, blocks_parent);
}

void __ompc_task_wait(){
    hpx_backend->task_wait();
}

void __ompc_task_exit(){
}

void __ompc_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}
void __ompc_end_serialized_parallel(int global_tid) {
    //It appears this function does nothing
}

void __ompc_critical(int gtid, int **lck) {
    if(*lck == NULL){
        *lck = new int;
        **lck = hpx_backend->new_mtx();
    }
    hpx_backend->lock(**lck);
}

void __ompc_end_critical(omp_int32 gtid, omp_int32 **lck) {
    hpx_backend->unlock(**lck);
}

omp_int32 __ompc_copyin_thdprv(int num,...) {
    return 0;
}

omp_int32 __ompc_copyprivate( omp_int32 mpsp_status,
                              void *cppriv, 
                              void(*cp)(void* src, void* dst) ) {
    return 0;
}
//OMP Library functions
//TODO: move to another file
int omp_get_num_threads() {
    return hpx_backend->get_num_threads();
}

int omp_get_max_threads() {
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
    int new_id = hpx_backend->new_mtx();
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
