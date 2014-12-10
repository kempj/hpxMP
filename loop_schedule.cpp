#include <iostream>
#include "loop_schedule.h"
//#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;

using std::cout;
using std::endl;


template<typename T, typename D=T>
void omp_static_init( int global_tid, int schedtype, int *plastiter,
                      T *p_lower, T *p_upper,
                      D *p_stride, D incr, D chunk) {
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    int block_size, stride, my_lower, my_upper;
    int team_size = loop_sched->num_threads;
    //if(team_size == 1) {*plastiter = 1;return;}
    int trip_count = (*p_upper - *p_lower) / incr + 1;
    int adjustment = ((trip_count % team_size) == 0) ? -1 : 0;

    if (schedtype == kmp_sch_static) {//TODO: is this correct for negative incr?
        *plastiter = ( global_tid == trip_count - 1 );

        stride = (trip_count / team_size + adjustment + 1) * incr;
        block_size = (trip_count / team_size + adjustment) * incr;
        my_lower = *p_lower + global_tid * stride;
        my_upper = my_lower + block_size;
    } else { //kmp_sch_static_chunked
        block_size = (chunk - 1) * incr;
        stride = chunk * incr;
        my_lower = *p_lower + global_tid * stride;
        my_upper = my_lower + block_size;
        *p_stride = stride * team_size;
        if(*plastiter) {
            *plastiter = (global_tid == (trip_count - 1) / chunk) % team_size;
        }
    }
    *p_lower = my_lower;
    *p_upper = my_upper;
}

/*
template<typename T>
void scheduler_init( int global_tid, int schedtype, int *plastiter,
                     T lower, T upper, T stride, T chunk) {
    auto loop_sched = &(hpx_backend->get_team()->loop_sched);
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

template<typename T, typename D=T>
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

                omp_static_init<T,D>( loop_sched->schedule_count, static_cast<omp_sched_t>(loop_sched->schedule),
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
*/

void
__kmpc_for_static_init_4( ident_t *loc, kmp_int32 global_tid, kmp_int32 schedtype, 
                          kmp_int32 *plastiter,kmp_int32 *p_lower, kmp_int32 *p_upper, 
                          kmp_int32 *p_stride, kmp_int32 incr, kmp_int32 chunk ) 
{
    omp_static_init<int>( global_tid, schedtype, plastiter, p_lower, p_upper,
                          p_stride, incr, chunk );
}

void
__kmpc_for_static_init_4u( ident_t *loc, kmp_int32 global_tid, kmp_int32 schedtype,
                           kmp_int32 *plastiter, uint32_t *p_lower, uint32_t *p_upper,
                           kmp_int32 *p_stride, kmp_int32 incr, kmp_int32 chunk )
{
    omp_static_init<uint32_t, int>( global_tid, schedtype, plastiter,
                                    p_lower, p_upper, p_stride, incr, chunk );
}

void 
__kmpc_dispatch_init_4( ident_t *loc, kmp_int32 global_tid, enum sched_type schedule,
                        kmp_int32 lb, kmp_int32 ub, kmp_int32 st, kmp_int32 chunk ){
}

void
__kmpc_for_static_fini( ident_t *loc, kmp_int32 global_tid ){
    //Only seems to do internal tracking in intel runtime
}
