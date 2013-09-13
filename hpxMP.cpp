#include "hpxMP.h"
#include "stdio.h"
#include <hpxc/threads.h>
/*
typedef int omp_int32;
typedef long long omp_int64;
typedef void *frame_pointer_t;
typedef omp_int32 omp_tid;
typedef void (*omp_micro)(omp_int32 , frame_pointer_t);
*/

#include <hpx/hpx.hpp>
#include <hpx/hpx_init.hpp>


void (*omp_task)(int, void*)=0;
int num_threads = 0;

void conversion_func(void*) {
//    omp_task(NULL);
}

int hpx_main() {
    hpxc_thread_t *threads = new hpxc_thread_t[num_threads];
    printf("hello from hpx main\n");
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
    printf("hello from fork\n");
  /*  omp_task = micro_task;
    num_threads = Nthreads;
    hpx::init();
*/
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
    return 1;
}

omp_int32 __ompc_get_local_thread_num()
{
    return 0;
}

