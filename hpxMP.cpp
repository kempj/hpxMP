#include "hpxMP.h"
//#include "stdio.h"
#include <iostream>
#include <hpxc/threads.h>
/*
typedef int omp_int32;
typedef long long omp_int64;
typedef void *frame_pointer_t;
typedef omp_int32 omp_tid;
typedef void (*omp_micro)(omp_int32 , frame_pointer_t);
*/

#include <hpx/hpx.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>

using namespace::std;

hpxc_thread_t *threads;

void (*omp_task)(int, void*)=0;
int num_threads = 0;

void conversion_func(void*) {
    omp_task(0, NULL);
}

int get_num_threads() {
    //TODO: first, read OMP_NUM_THREADS from env
    return hpx::threads::hardware_concurrency();
}

int hpx_main() {
    threads = new hpxc_thread_t[num_threads];
    cout << "hello from hpx main" << endl;
    for(int i = 0; i < num_threads; i++) {
        hpxc_thread_create(&threads[i], 0, (void* (*)(void*))conversion_func, 0);
    }
    for(int i = 0; i < num_threads; i++) {
        hpxc_thread_join(threads[i], 0);
    }
    return hpx::finalize();
}

void __ompc_fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp)
{
    omp_task = micro_task;
    num_threads = get_num_threads();
    cout << "hello from fork ( " << num_threads << " threads)" << endl;
    hpx::init();

}

void __ompc_serialized_parallel(omp_int32 global_tid)
{
    //It appears this function does nothing
}

void __ompc_end_serialized_parallel(omp_int32 global_tid)
{
    //It appears this function does nothing
}

void __ompc_task_exit()
{
}

omp_int32 __ompc_can_fork()
{
    //some logic here to detect if HPX can spawn threads?
    return 1;
}

omp_int32 __ompc_get_local_thread_num()
{
    for(int i = 0; i < num_threads; i++) {
        if(hpxc_thread_equal(hpxc_thread_self(), threads[i])) {
            return i;
        }
    }
    return 0;
}

void __ompc_static_init_4(omp_int32 global_tid, omp_sched_t schedtype,
        omp_int32 *p_lower,
        omp_int32 *p_upper, omp_int32 *p_stride,
        omp_int32 incr, omp_int32 chunk) 
{
    int thread_num = __ompc_get_local_thread_num();
    int size = *p_upper - *p_lower + 1;
    int chunk_size = size/num_threads;
    if(thread_num < size % num_threads) {
        *p_lower = thread_num * (chunk_size+1);
        *p_upper = *p_lower + chunk_size ;
    } else {
        *p_lower = (size % num_threads) * (chunk_size+1) + (thread_num - size % num_threads ) * chunk_size;
        *p_upper = *p_lower + chunk_size - 1;
    }
}

