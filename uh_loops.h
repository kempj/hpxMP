#include <cstdint>

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
