typedef int omp_int32;
typedef long long omp_int64;

typedef void *frame_pointer_t;
typedef omp_int32 omp_tid;
typedef void (*omp_micro)(omp_int32 , frame_pointer_t);

typedef enum {
    OMP_SCHED_UNKNOWN             = 0,
    OMP_SCHED_STATIC              = 1,
    OMP_SCHED_STATIC_EVEN         = 2,
    OMP_SCHED_DYNAMIC             = 3,
    OMP_SCHED_GUIDED              = 4,
    OMP_SCHED_RUNTIME             = 5,

    OMP_SCHED_ORDERED_STATIC      = 31,
    OMP_SCHED_ORDERED_STATIC_EVEN = 32,
    OMP_SCHED_ORDERED_DYNAMIC     = 33,
    OMP_SCHED_ORDERED_GUIDED  = 34,
    OMP_SCHED_ORDERED_RUNTIME     = 35
} omp_sched_t;


extern "C" void __ompc_fork(int num_threads, omp_micro micro_task,
                      frame_pointer_t fp);

extern "C" void __ompc_serialized_parallel(omp_int32 global_tid);
extern "C" void __ompc_end_serialized_parallel(omp_int32 global_tid);

extern "C" void __ompc_task_exit();

extern "C" omp_int32 __ompc_can_fork();

extern "C" omp_int32 __ompc_get_local_thread_num();

extern "C" void __ompc_static_init_4( omp_int32 global_tid, omp_sched_t schedtype,
                                      omp_int32 *plower,
                                      omp_int32 *pupper, omp_int32 *pstride,
                                      omp_int32 incr, omp_int32 chunk);

