//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include "hpxMP.h"
#include <iostream>
#include <cstdlib>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>

using namespace std;
boost::shared_ptr<hpx_runtime> hpx_backend;

bool started = false;

//overwrites global in openmp
int __ompc_init_rtl(int num_threads) {
    return 0;
}

void start_backend(){
    if( !hpx::get_runtime_ptr() ) {
        hpx_backend.reset(new hpx_runtime());
    } else {
        //__ompc_fork was called from an hpx thread without the hpx_backend existing,
        // most likely the application started hpx, and had omp calls in it.
        // TODO: set up the runtime without trying to start hpx
        assert(false);
        }
}

void __ompc_fork(int nthreads, omp_micro micro_task, frame_pointer_t fp) {
    assert(nthreads >= 0);
    if(!hpx_backend) {
        start_backend();
    }
    assert(!started);//Nested parallelism is disabled
    started = true;
    hpx_backend->fork(nthreads, micro_task, fp);
    started = false;
}

int __ompc_can_fork() {
    return !started;
}

void __ompc_reduction(int gtid, omp_lock_t **lck){
    __ompc_critical(gtid, lck);
}

void __ompc_end_reduction(int gtid, omp_lock_t **lck){
    __ompc_end_critical(gtid, lck);
}

void __ompc_barrier() {
    __ompc_ebarrier();
}

void __ompc_ebarrier() {
    if(started)
        hpx_backend->barrier_wait();
}

int __ompc_get_num_threads(){
    if(started)
        return hpx_backend->get_num_threads();
    return 0;
}

int __ompc_master(int global_tid){
    //TODO: if master can be called from tasks, then this doesn't work.
    if(__ompc_get_local_thread_num() == 0) 
        return 1;
    return 0;
}

void __ompc_end_master(int global_tid){
}

int __ompc_single(int tid){
    if(!started)
        return 1;

    parallel_region *team = hpx_backend->get_team();

    int num_threads = __ompc_get_num_threads();
    team->single_mtx.lock();
    if(team->current_single_thread == -1 && team->single_counter == 0) {
        team->current_single_thread = tid;
        team->single_counter = 1 - num_threads;
    } else {
        team->single_counter++;
    }
    team->single_mtx.unlock();
    if(team->current_single_thread == tid) {
        return 1;
    }
    return 0;
}

void __ompc_end_single(int tid){
    parallel_region *team = hpx_backend->get_team();
    if(!started)
        return;
    team->single_mtx.lock();
    if(team->single_counter == 0) {
        team->current_single_thread = -1;
    }
    team->single_mtx.unlock();
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
    // blocks_parent keeps parent tasks from exiting when they have shared 
    // variables in nested tasks.
    if(may_delay == 0 || !started) {
       task_func(firstprivates, frame_pointer);
    } else {
        hpx_backend->create_task( task_func, frame_pointer, firstprivates,
                                  is_tied, blocks_parent);
    }
}

void __ompc_task_wait(){
    if(started)
        hpx_backend->task_wait();
}

void __ompc_task_exit(){
    if(started)
        hpx_backend->task_exit();
}

void __ompc_serialized_parallel(int global_tid) {
}
void __ompc_end_serialized_parallel(int global_tid) {
}
//Note: volatile was removed from all the omp_lock_t calls
// and const_cast can get rid of the volatile if needed
void __ompc_critical(int gtid, omp_lock_t **lck) {
    if(!started)
        return;

    parallel_region *team = hpx_backend->get_team();

    omp_lock_t* tmp_mtx = new omp_lock_t;
    if(*lck == NULL ) {
        //FIXME:
        //hpx_backend->crit_mtx.lock();
        team->crit_mtx.lock();
        if(*lck == NULL ){
            *lck = tmp_mtx;
        }
        team->crit_mtx.unlock();
    } 
    if(&(*tmp_mtx) != &(**lck)) {
        delete tmp_mtx;
    }
    (**lck).lock();
}

void __ompc_end_critical(int gtid, omp_lock_t **lck) {
    if(started)
        (**lck).unlock();
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

int __ompc_get_local_thread_num() {
    if(hpx_backend && started) {
        return hpx_backend->get_thread_num();
    }
    return 0;
}

//beginning of loop calls
void __ompc_ordered(int global_tid){
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    while(loop_sched->ordered_count != loop_sched->local_iter[global_tid]){
        loop_sched->yield();
    }
}

void __ompc_end_ordered(int global_tid){
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    loop_sched->iter_remaining[global_tid]--;
    if(loop_sched->iter_remaining[global_tid] <= 0) {
        loop_sched->ordered_count++;
    }
}

template<typename T>
void scheduler_init( int global_tid, omp_sched_t schedtype, 
                     T lower, T upper, T stride, T chunk, loop_data *loop_sched) {
    // waiting for last loop to finish.
    while(!loop_sched->work_remains && loop_sched->num_workers > 0 ) {
        loop_sched->yield();
    }
    int NT = loop_sched->num_threads;
    loop_sched->lock();
    if(loop_sched->num_workers == 0) {
        loop_sched->work_remains = true;
        loop_sched->lower = lower;
        loop_sched->upper = upper;
        loop_sched->stride = stride;
        loop_sched->chunk = chunk;
        loop_sched->schedule = static_cast<int>(schedtype);
        loop_sched->ordered_count = 0;
        loop_sched->schedule_count = 0;
        loop_sched->num_threads = NT;
        loop_sched->local_iter.resize(NT);
        loop_sched->iter_remaining.resize(NT);
        loop_sched->num_workers++;
    } else {
        loop_sched->num_workers++;
    }
    loop_sched->iter_remaining[global_tid] = 0;
    loop_sched->local_iter[global_tid] = 0;
    loop_sched->unlock();
}

template<typename T, typename D=T>
void omp_static_init( int global_tid, omp_sched_t schedtype, 
                      T *p_lower, T *p_upper,
                      D *p_stride, D incr, D chunk, loop_data *loop_sched) {
    int block_size, stride, my_lower, my_upper;
    int team_size = loop_sched->num_threads;
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
}

template<typename T, typename D=T>
int omp_next(int global_tid, T *p_lower, T *p_upper, T *p_stride, loop_data *loop_sched) {

    switch (static_cast<omp_sched_t>(loop_sched->schedule)) {
        case OMP_SCHED_STATIC_EVEN: //STATIC_EVEN uses default chunking.
        case OMP_SCHED_STATIC: //STATIC_EVEN can have user specified chunking.
        case OMP_SCHED_ORDERED_STATIC_EVEN:
        case OMP_SCHED_ORDERED_STATIC:
            //FIXME: static ordered does not work. Each thread takes only one chunk and no more.
            loop_sched->lock();
            if(loop_sched->schedule_count < loop_sched->num_threads && loop_sched->work_remains) {
                loop_sched->local_iter[global_tid] = loop_sched->schedule_count;
                loop_sched->schedule_count++;
                loop_sched->unlock();
                *p_lower= loop_sched->lower;
                *p_upper= loop_sched->upper;

                omp_static_init<T,D>( loop_sched->schedule_count, static_cast<omp_sched_t>(loop_sched->schedule),
                                    p_lower, p_upper, p_stride, 
                                    loop_sched->stride, loop_sched->chunk, loop_sched);
                loop_sched->iter_remaining[global_tid] = (*p_upper - *p_lower) / *p_stride + 1;
                return 1;
            } 
            loop_sched->unlock();
            //Wait for every thread to at least start the loop before exiting
            while( loop_sched->num_workers < loop_sched->num_threads &&
                        loop_sched->work_remains){
                loop_sched->yield();
            }
            loop_sched->lock();
            loop_sched->work_remains = false;
            loop_sched->num_workers--;
            loop_sched->unlock();
            return 0;

        case OMP_SCHED_GUIDED:
        case OMP_SCHED_DYNAMIC:
        case OMP_SCHED_ORDERED_DYNAMIC:
        case OMP_SCHED_ORDERED_GUIDED:
        case OMP_SCHED_RUNTIME:
        case OMP_SCHED_ORDERED_RUNTIME:
            if((loop_sched->upper - loop_sched->lower) * loop_sched->stride < 0 ) {
                loop_sched->unlock();
                while( loop_sched->num_workers < loop_sched->num_threads &&
                       loop_sched->work_remains){
                    loop_sched->yield();
                }
                loop_sched->lock();
                loop_sched->work_remains = false;
                loop_sched->num_workers--;
                loop_sched->unlock();
                return 0;
            }
            *p_lower = loop_sched->lower;
            *p_stride = loop_sched->stride;
            *p_upper = *p_lower + (loop_sched->chunk -1) * (*p_stride);
            loop_sched->lower = *p_upper + *p_stride;
            loop_sched->unlock();
            loop_sched->local_iter[global_tid] = loop_sched->schedule_count;
            loop_sched->iter_remaining[global_tid] = loop_sched->chunk;
            loop_sched->schedule_count++;
            return 1;

        default:
            if(global_tid == 0)
                cout << "default" << endl;
    }
    return 0;
}

int __ompc_schedule_next_8( int global_tid,
                            int64_t *plower, int64_t *pupper, 
                            int64_t *pstride){
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    return omp_next<int64_t>(global_tid, plower, pupper, pstride, loop_sched);
}

int __ompc_schedule_next_4( int global_tid,
                            int *plower, int *pupper, int *pstride){
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    return omp_next<int>(global_tid, plower, pupper, pstride, loop_sched);
}

void __ompc_scheduler_init_8( int global_tid, omp_sched_t schedtype,
                              int64_t lower, int64_t upper,
                              int64_t stride, int64_t chunk) {
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    scheduler_init<int64_t>( global_tid, schedtype, lower, upper, stride, 
                             chunk, loop_sched);
}

void __ompc_scheduler_init_4( int global_tid,
                              omp_sched_t schedtype,
                              int lower, int upper,
                              int stride, int chunk){
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    scheduler_init<int>( global_tid, schedtype, lower, upper, stride, 
                         chunk, loop_sched);
}

void __ompc_static_init_8( int global_tid, omp_sched_t schedtype,
                           int64_t *p_lower, int64_t *p_upper, 
                           int64_t *p_stride, int64_t incr, int64_t chunk ) {
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    omp_static_init<int64_t>( global_tid, schedtype, p_lower, p_upper, 
                              p_stride, incr, chunk, loop_sched);
}

void __ompc_static_init_4( int global_tid, omp_sched_t schedtype,
                           int *p_lower, int *p_upper, 
                           int *p_stride, int incr, int chunk) {
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    omp_static_init<int>( global_tid, schedtype, p_lower, p_upper,
                          p_stride, incr, chunk, loop_sched);
}
//End loop calls

//begin library calls
//OpenMP 3.1 spec, section 3.2.1
#pragma weak omp_set_num_threads_ = omp_set_num_threads
void omp_set_num_threads(int nthreads){
    hpx_backend->set_num_threads(nthreads);
}

//OpenMP 3.1 spec, section 3.2.2
//can be called from outside a parallel region
#pragma weak omp_get_num_threads_ = omp_get_num_threads
int omp_get_num_threads() {
    if(hpx_backend && started)
        return hpx_backend->get_num_threads();
    return 1;
}

//OpenMP 3.1 spec, section 3.2.3
#pragma weak omp_get_max_threads_ = omp_get_max_threads
int omp_get_max_threads() {
    if(!hpx_backend) {
        start_backend();
    }                      
    //return hpx_backend->get_num_threads();
    return hpx_backend->get_num_procs();
}

//OpenMP 3.1 spec, section 3.2.4
#pragma weak omp_get_thread_num_ = omp_get_thread_num
int omp_get_thread_num() {
    return __ompc_get_local_thread_num();
}

//OpenMP 3.1 spec, section 3.2.5
#pragma weak omp_get_num_procs_ = omp_get_num_procs
int omp_get_num_procs() {
    if(!hpx_backend) {
        start_backend();
    }
    return hpx_backend->get_num_procs();
}

//OpenMP 3.1 spec, section 3.2.6
int omp_in_parallel(){
    return started;
}

//OpenMP 3.1 spec, section 3.2.7
void omp_set_dynamic(int dynamic_threads){
    if(!hpx_backend) {
        start_backend();
    }
    hpx_backend->get_task_data()->icv.dyn = (dynamic_threads != 0);
}

//The omp_set_dynamic routine enables or disables dynamic adjustment of the
//number of threads available for the execution of subsequent parallel regions by
//setting the value of the dyn-var ICV.

//OpenMP 3.1 spec, section 3.2.8
int omp_get_dynamic(void) {
    if(!hpx_backend) {
        start_backend();
    }
    return hpx_backend->get_task_data()->icv.dyn;
}
//OpenMP 3.1 spec, section 3.2.9
void omp_set_nested(int nested){
}
//OpenMP 3.1 spec, section 3.2.10
//int omp_get_nested(void);
//OpenMP 3.1 spec, section 3.2.11
//void omp_set_schedule(omp_sched_t kind, int modifier);
//OpenMP 3.1 spec, section 3.2.12
//void omp_get_schedule(omp_sched_t * kind, int * modifier );
//OpenMP 3.1 spec, section 3.2.13
//int omp_get_thread_limit(void);
//OpenMP 3.1 spec, section 3.2.14
//void omp_set_max_active_levels (int max_levels);
//OpenMP 3.1 spec, section 3.2.15
//int omp_get_max_active_levels(void);
//OpenMP 3.1 spec, section 3.2.16
//int omp_get_level (void);
//OpenMP 3.1 spec, section 3.2.17
//int omp_get_ancestor_thread_num (int level);
//OpenMP 3.1 spec, section 3.2.18
//int omp_get_team_size (int level);
//OpenMP 3.1 spec, section 3.2.19
//int omp_get_active_level (void);
//OpenMP 3.1 spec, section 3.2.20
//int omp_in_final(void);

//TODO: I am pretty sure these all need to be pointers to pointers
//OpenMP 3.1 spec, section 3.3.1
void omp_init_lock(omp_lock_t *lock) {
    if(!hpx_backend) {
        start_backend();
    }
    lock = new omp_lock_t;
}

void omp_init_nest_lock(omp_nest_lock_t *lock) {
}

//OpenMP 3.1 spec, section 3.3.2
void omp_destroy_lock(omp_lock_t *lock) {
    delete lock;
}

void omp_destroy_nest_lock(omp_nest_lock_t *lock) {
}

//OpenMP 3.1 spec, section 3.3.3
void omp_set_lock(omp_lock_t *lock) {
    lock->lock();
}

void omp_set_nest_lock(omp_nest_lock_t *lock) {
}

//OpenMP 3.1 spec, section 3.3.4
void omp_unset_lock(omp_lock_t *lock) {
    lock->unlock();
}

void omp_unset_nest_lock(omp_nest_lock_t *lock) {
}

//OpenMP 3.1 spec, section 3.3.5
int omp_test_lock(omp_lock_t *lock) {
    if(lock->try_lock())
        return 1;
    return 0;
}

int omp_test_nest_lock(omp_nest_lock_t *lock) {
    return 1;
}

//OpenMP 3.1 spec, section 3.4.1
double omp_get_wtime() {
    if(!hpx_backend) {
        start_backend();
    }
    return hpx_backend->get_time();
}

//OpenMP 3.1 spec, section 3.4.2
double omp_get_wtick() {
    //high resolution elapsed_min
    return .000000001;
}

