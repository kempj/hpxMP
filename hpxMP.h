//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <time.h>
#include "hpx_runtime.h"

typedef int omp_int32;
typedef long long omp_int64;

typedef void *frame_pointer_t;
typedef int omp_tid;
typedef void (*omp_micro)(int , frame_pointer_t);

typedef void (*omp_task_func)(void *firstprivates, void *fp);

typedef mutex_type omp_lock_t;
typedef void *omp_nest_lock_t;

extern "C" int __ompc_init_rtl(int num_threads);
extern "C" void __ompc_critical(int gtid, omp_lock_t **lck);
extern "C" void __ompc_end_critical(int gtid, omp_lock_t **lck);

extern "C" void __ompc_fork(int num_threads, omp_micro micro_task,
                      frame_pointer_t fp);

extern "C" void __ompc_serialized_parallel(int global_tid);
extern "C" void __ompc_end_serialized_parallel(int global_tid);

extern "C" void __ompc_task_exit();

extern "C" int __ompc_can_fork();

extern "C" int __ompc_get_local_thread_num();

extern "C" void __ompc_ordered(omp_int32 global_tid);
extern "C" void __ompc_end_ordered(omp_int32 global_tid);

extern "C" void __ompc_ebarrier();
extern "C" void __ompc_barrier();

extern "C" int __ompc_get_num_threads();

extern "C" int __ompc_master(int global_tid);
extern "C" void __ompc_end_master(int global_tid);

extern "C" int __ompc_single(int global_tid);
extern "C" void __ompc_end_single(int global_tid);

extern "C" int __ompc_task_will_defer(int may_delay);
extern "C" void __ompc_task_create(omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int may_delay, 
                               int is_tied, int blocks_parent);
extern "C" void __ompc_task_wait();
extern "C" void __ompc_task_exit();

extern "C" void __ompc_task_firstprivates_alloc(void **firstprivates, int size);
extern "C" void __ompc_task_firstprivates_free(void *firstprivates);

extern "C" void __ompc_reduction(omp_int32 gtid, omp_lock_t **lck);
extern "C" void __ompc_end_reduction(omp_int32 gtid, omp_lock_t **lck);

extern "C" omp_int32 __ompc_get_thdprv( void *** thdprv_p, omp_int64 size, 
                                        void *datap, omp_int32 global_tid);

extern "C" omp_int32 __ompc_copyin_thdprv(int num,...);
extern "C" omp_int32 __ompc_copyprivate( omp_int32 mpsp_status,
                                         void *cppriv, 
                                         void(*cp)(void* src, void* dst) );


extern "C" int omp_get_num_threads();
//#pragma weak omp_get_num_threads_ = omp_get_num_threads

extern "C" void omp_set_num_threads(int);
//#pragma weak omp_set_num_threads_ = omp_set_num_threads

extern "C" int omp_get_max_threads();

extern "C" int omp_get_max_threads_();
//#pragma weak omp_get_max_threads_ = omp_get_max_threads

extern "C" int omp_get_thread_num();

extern "C" int omp_get_thread_num_();
//#pragma weak omp_get_thread_num_ = omp_get_thread_num

extern "C" int omp_get_num_procs();
//#pragma weak omp_get_num_procs_ = omp_get_num_procs

//extern "C" timespec get_hpx_time();
extern "C" double omp_get_wtime();
extern "C" double omp_get_wtick();
extern "C" void omp_set_nested();
extern "C" int omp_in_parallel();
extern "C" void omp_set_dynamic(int dynamic_threads);
extern "C" void omp_get_dynamic(int dynamic_threads);


extern "C" void omp_init_lock(omp_lock_t *lock);
extern "C" void omp_init_nest_lock(omp_nest_lock_t *lock);

extern "C" void omp_destroy_lock(omp_lock_t *lock);
extern "C" void omp_destroy_nest_lock(omp_nest_lock_t *lock);

extern "C" void omp_set_lock(omp_lock_t *lock);
extern "C" void omp_set_nest_lock(omp_nest_lock_t *lock);

extern "C" void omp_unset_lock(omp_lock_t *lock);
extern "C" void omp_unset_nest_lock(omp_nest_lock_t *lock);

extern "C" int omp_test_lock(omp_lock_t *lock);
extern "C" int omp_test_nest_lock(omp_nest_lock_t *lock);


typedef enum {
    OMP_SCHED_UNKNOWN             = 0,
    OMP_SCHED_STATIC              = 1,  //user specified chunking
    OMP_SCHED_STATIC_EVEN         = 2,  //default chunking
    OMP_SCHED_DYNAMIC             = 3,  //dynamic
    OMP_SCHED_GUIDED              = 4,
    OMP_SCHED_RUNTIME             = 5,

    OMP_SCHED_ORDERED_RUNTIME     = 31,//guess
    OMP_SCHED_ORDERED_GUIDED      = 32,//guess
    OMP_SCHED_ORDERED_STATIC      = 33,
    OMP_SCHED_ORDERED_STATIC_EVEN = 34,
    OMP_SCHED_ORDERED_DYNAMIC     = 35
} omp_sched_t;

extern "C" void 
__ompc_static_init_4( int global_tid, omp_sched_t schedtype,
                      int *plower, int *pupper, int *pstride,
                      int incr, int chunk);

extern "C" void 
kmp_static_init_4u( int global_tid, omp_sched_t schedtype,
                    uint32_t *plower, uint32_t *pupper, int *pstride,
                    int incr, int chunk);
extern "C" void 
__ompc_static_init_8( int global_tid, omp_sched_t schedtype,
                      int64_t *plower, int64_t *pupper, int64_t *pstride,
                      int64_t incr, int64_t chunk );

extern "C" void 
__ompc_scheduler_init_4( int global_tid, omp_sched_t schedtype,
                         int lower, int upper,
                         int stride, int chunk);

extern "C" void 
__ompc_scheduler_init_8( int global_tid, omp_sched_t schedtype,
                         int64_t lower, int64_t upper,
                         int64_t stride, int64_t chunk);
extern "C" int 
__ompc_schedule_next_4( int global_tid, int *plower, int *pupper, int *pstride);

extern "C" int 
__ompc_schedule_next_8( int global_tid, int64_t *plower, int64_t *pupper, int64_t *pstride);
