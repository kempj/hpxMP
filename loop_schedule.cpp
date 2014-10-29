#include <iostream>
#include "loop_schedule.h"
//#include "par_region.h"
#include "hpx_runtime.h"
//#include "loop_data.h"
//#include "boost/shared_ptr.hpp"

//extern boost::shared_ptr<loop_data> loop_sched;
extern boost::shared_ptr<hpx_runtime> hpx_backend;

using std::cout;
using std::endl;
struct ident_t;


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
    while(loop_sched->is_done && loop_sched->num_workers > 0 ) {
        loop_sched->yield();
    }
    int NT = loop_sched->num_threads;
    loop_sched->lock();
    if(loop_sched->num_workers == 0) {
        loop_sched->is_done = false;
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

template<typename T>
void omp_static_init( int global_tid, omp_sched_t schedtype, 
                      T *p_lower, T *p_upper,
                      T * p_stride, T incr, T chunk, loop_data *loop_sched) {
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

template<typename T>
int omp_next(int global_tid, T *p_lower, T *p_upper, T *p_stride, loop_data *loop_sched) {

    switch (static_cast<omp_sched_t>(loop_sched->schedule)) {
        case OMP_SCHED_STATIC_EVEN: //STATIC_EVEN uses default chunking.
        case OMP_SCHED_STATIC: //STATIC_EVEN can have user specified chunking.
        case OMP_SCHED_ORDERED_STATIC_EVEN:
        case OMP_SCHED_ORDERED_STATIC:
            loop_sched->lock();
            if(loop_sched->schedule_count < loop_sched->num_threads && !loop_sched->is_done) {
                loop_sched->local_iter[global_tid] = loop_sched->schedule_count;
                loop_sched->schedule_count++;
                loop_sched->unlock();
                *p_lower= loop_sched->lower;
                *p_upper= loop_sched->upper;

                omp_static_init<T>( loop_sched->schedule_count, static_cast<omp_sched_t>(loop_sched->schedule),
                                    p_lower, p_upper, p_stride, 
                                    loop_sched->stride, loop_sched->chunk, loop_sched);
                loop_sched->iter_remaining[global_tid] = (*p_upper - *p_lower) / *p_stride + 1;
                return 1;
            } 
            loop_sched->unlock();
            //Wait for every thread to at least start the loop before exiting
            while( loop_sched->num_workers < loop_sched->num_threads &&
                        !loop_sched->is_done){
                loop_sched->yield();
            }
            loop_sched->lock();
            loop_sched->is_done = true;
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
                       !loop_sched->is_done){
                    loop_sched->yield();
                }
                loop_sched->lock();
                loop_sched->is_done = true;
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
