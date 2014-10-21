#include <iostream>

using std::cout;
using std::endl;
struct ident_t;

loop_data loop_sched;


int __ompc_schedule_next_8( int global_tid,
                                  int64_t *plower, int64_t *pupper,
                                  int64_t *pstride){
    cout << "Not implemented: __ompc_schedule_next_8" << endl;
    return 0;
}

int __ompc_schedule_next_4( int global_tid,
                                  int *p_lower, int *p_upper,
                                  int *p_stride){

}

void __ompc_scheduler_init_8( int global_tid, omp_sched_t schedtype,
                              int64_t lower, int64_t upper,
                              int64_t stride, int64_t chunk){
    cout << "Not implemented: __ompc_scheduler_init_8" << endl;
}

void __ompc_scheduler_init_4( int global_tid,
                              omp_sched_t schedtype,
                              int lower, int upper,
                              int stride, int chunk){
    // waiting for last loop to finish.
    while(loop_sched.is_done && loop_sched.num_workers > 0 ) {
        hpx::this_thread::yield();
    }
    int NT = __ompc_get_num_threads();
    loop_mtx->lock();
    if(loop_sched.num_workers == 0) {
        loop_sched.is_done = false;
        loop_sched.lower = lower;
        loop_sched.upper = upper;
        loop_sched.stride = stride;
        loop_sched.chunk = chunk;
        loop_sched.schedule = static_cast<int>(schedtype);
        loop_sched.ordered_count = 0;
        loop_sched.schedule_count = 0;
        loop_sched.num_threads = NT;
        loop_sched.local_iter.resize(NT);
        loop_sched.iter_remaining.resize(NT);
        loop_sched.num_workers++;
    } else {
        loop_sched.num_workers++;
    }
    loop_sched.iter_remaining[global_tid] = 0;
    loop_sched.local_iter[global_tid] = 0;
    loop_mtx->unlock();
}

void __ompc_static_init_8( int global_tid, omp_sched_t schedtype,
                      int64_t *p_lower, int64_t *p_upper, int64_t *p_stride,
                      int64_t incr, int64_t chunk ){
    omp_static_init<uint64_t>( global_tid, schedtype, p_lower, p_upper, p_stride,
                               incr, chunk);
}

void __ompc_static_init_4( int global_tid, omp_sched_t schedtype,
                           int *p_lower, int *p_upper, 
                           int *p_stride, int incr, 
                           int chunk) {
    omp_static_init<int>( global_tid, schedtype, p_lower, p_upper, p_stride,
                               incr, chunk);
}

template<typename T>
void omp_static_init( int tid, omp_sched_t schedtype, T *p_lower, T *p_upper,
                 T * p_stride, int incr, int chunk) 
{
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
}

template<typename T>
omp_for_next(int tid, T *p_lower, T *p_upper, T *p_stride) {
    switch (static_cast<omp_sched_t>(loop_sched.schedule)) {
        case OMP_SCHED_STATIC_EVEN: //STATIC_EVEN uses default chunking.
        case OMP_SCHED_STATIC: //STATIC_EVEN can have user specified chunking.
        case OMP_SCHED_ORDERED_STATIC_EVEN:
        case OMP_SCHED_ORDERED_STATIC:
            loop_mtx->lock();
            if(loop_sched.schedule_count < loop_sched.num_threads && !loop_sched.is_done) {
                loop_sched.local_iter[global_tid] = loop_sched.schedule_count;
                loop_sched.schedule_count++;
                loop_mtx->unlock();
                *p_lower= loop_sched.lower;
                *p_upper= loop_sched.upper;

                __ompc_static_init_4( loop_sched.schedule_count, static_cast<omp_sched_t>(loop_sched.schedule),
                                      p_lower, p_upper, p_stride, 
                                      loop_sched.stride, loop_sched.chunk);
                loop_sched.iter_remaining[global_tid] = (*p_upper - *p_lower) / *p_stride + 1;
                return 1;
            } 
            loop_mtx->unlock();
            //Wait for every thread to at least start the loop before exiting
            while( loop_sched.num_workers < loop_sched.num_threads &&
                        !loop_sched.is_done){
                hpx::this_thread::yield();
            }
            loop_mtx->lock();
            loop_sched.is_done = true;
            loop_sched.num_workers--;
            loop_mtx->unlock();
            return 0;

        case OMP_SCHED_GUIDED:
        case OMP_SCHED_DYNAMIC:
        case OMP_SCHED_ORDERED_DYNAMIC:
        case OMP_SCHED_ORDERED_GUIDED:
        case OMP_SCHED_RUNTIME:
        case OMP_SCHED_ORDERED_RUNTIME:
            if((loop_sched.upper - loop_sched.lower) * loop_sched.stride < 0 ) {
                loop_mtx->unlock();
                while( loop_sched.num_workers < loop_sched.num_threads &&
                       !loop_sched.is_done){
                    hpx::this_thread::yield();
                }
                loop_mtx->lock();
                loop_sched.is_done = true;
                loop_sched.num_workers--;
                loop_mtx->unlock();
                return 0;
            }
            *p_lower = loop_sched.lower;
            *p_stride = loop_sched.stride;
            *p_upper = *p_lower + (loop_sched.chunk -1) * (*p_stride);
            loop_sched.lower = *p_upper + *p_stride;
            loop_mtx->unlock();
            loop_sched.local_iter[global_tid] = loop_sched.schedule_count;
            loop_sched.iter_remaining[global_tid] = loop_sched.chunk;
            loop_sched.schedule_count++;
            return 1;

        default:
            if(global_tid == 0)
                cout << "default" << endl;
    }
    return 0;
}
