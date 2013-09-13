typedef int omp_int32;
typedef long long omp_int64;

typedef void *frame_pointer_t;
typedef omp_int32 omp_tid;
typedef void (*omp_micro)(omp_int32 , frame_pointer_t);



extern "C" void __ompc_fork(int num_threads, omp_micro micro_task,
                      frame_pointer_t fp);

extern "C" void __ompc_serialized_parallel(omp_int32 global_tid);
extern "C" void __ompc_end_serialized_parallel(omp_int32 global_tid);

extern "C" void __ompc_task_exit();

extern "C" omp_int32 __ompc_can_fork();

extern "C" omp_int32 __ompc_get_local_thread_num();


