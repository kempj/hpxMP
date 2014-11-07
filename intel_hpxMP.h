#include "hpx_runtime.h"
#include <cstdarg>

typedef int kmp_int32;
typedef long long kmp_int64;

typedef void                (*microtask_t)( int *gtid, int *npr, ... );
typedef int                 (*launch_t)( int gtid );

typedef void (*kmpc_micro)  ( kmp_int32 * global_tid, kmp_int32 * bound_tid, ... );

enum sched_type {
        kmp_sch_lower                     = 32,   /**< lower bound for unordered values */
        kmp_sch_static_chunked            = 33,
        kmp_sch_static                    = 34,   /**< static unspecialized */
        kmp_sch_dynamic_chunked           = 35,
        kmp_sch_guided_chunked            = 36,   /**< guided unspecialized */
        kmp_sch_runtime                   = 37,
        kmp_sch_auto                      = 38,   /**< auto */
        kmp_sch_trapezoidal               = 39,
        /* accessible only through KMP_SCHEDULE environment variable */
        kmp_sch_static_greedy             = 40,
        kmp_sch_static_balanced           = 41,
        /* accessible only through KMP_SCHEDULE environment variable */
        kmp_sch_guided_iterative_chunked  = 42,
        kmp_sch_guided_analytical_chunked = 43,
        kmp_sch_static_steal              = 44,   /**< accessible only through KMP_SCHEDULE environment variable */
        /* accessible only through KMP_SCHEDULE environment variable */
        kmp_sch_upper                     = 45,   /**< upper bound for unordered values */
        kmp_ord_lower                     = 64,   /**< lower bound for ordered values, must be power of 2 */
        kmp_ord_static_chunked            = 65,
        kmp_ord_static                    = 66,   /**< ordered static unspecialized */
        kmp_ord_dynamic_chunked           = 67,
        kmp_ord_guided_chunked            = 68,
        kmp_ord_runtime                   = 69,
        kmp_ord_auto                      = 70,   /**< ordered auto */
        kmp_ord_trapezoidal               = 71,
        kmp_ord_upper                     = 72,   /**< upper bound for ordered values */
#if OMP_40_ENABLED
        /* Schedules for Distribute construct */
        kmp_distribute_static_chunked     = 91,   /**< distribute static chunked */
        kmp_distribute_static             = 92,   /**< distribute static unspecialized */
#endif
        kmp_nm_lower                      = 160,  /**< lower bound for nomerge values */
        kmp_nm_static_chunked             = (kmp_sch_static_chunked - kmp_sch_lower + kmp_nm_lower),
        kmp_nm_static                     = 162,  /**< static unspecialized */
        kmp_nm_dynamic_chunked            = 163,
        kmp_nm_guided_chunked             = 164,  /**< guided unspecialized */
        kmp_nm_runtime                    = 165,
        kmp_nm_auto                       = 166,  /**< auto */
        kmp_nm_trapezoidal                = 167,
        /* accessible only through KMP_SCHEDULE environment variable */
        kmp_nm_static_greedy              = 168,
        kmp_nm_static_balanced            = 169,
        /* accessible only through KMP_SCHEDULE environment variable */
        kmp_nm_guided_iterative_chunked   = 170,
        kmp_nm_guided_analytical_chunked  = 171,
        kmp_nm_static_steal               = 172,  /* accessible only through OMP_SCHEDULE environment variable */
        kmp_nm_ord_static_chunked         = 193,
        kmp_nm_ord_static                 = 194,  /**< ordered static unspecialized */
        kmp_nm_ord_dynamic_chunked        = 195,
        kmp_nm_ord_guided_chunked         = 196,
        kmp_nm_ord_runtime                = 197,
        kmp_nm_ord_auto                   = 198,  /**< auto */
        kmp_nm_ord_trapezoidal            = 199,
        kmp_nm_upper                      = 200,  /**< upper bound for nomerge values */
        kmp_sch_default = kmp_sch_static  /**< default scheduling algorithm */
};



typedef struct ident {
    kmp_int32 reserved_1;   /**<  m ght be used in Fortran; see above  */
    kmp_int32 flags;        /**<  also f.flags; KMP_IDENT_xxx flags; KMP_IDENT_KMPC identifies this union member  */
    kmp_int32 reserved_2;   /**<  not really used in Fortran any more; see above */
    kmp_int32 reserved_3;   /**< source[4] in Fortran, do not use for C++  */
    char const *psource;    /**< String describing the source location.*/
} ident_t;

extern "C" void   __kmpc_fork_call          ( ident_t *, kmp_int32 nargs, kmpc_micro microtask, ... );
extern "C" int  __kmpc_global_thread_num(ident_t *loc);
extern "C" void __kmpc_push_num_threads ( ident_t *loc, kmp_int32 global_tid, kmp_int32 num_threads );
extern "C" int  __kmpc_cancel_barrier(ident_t* loc_ref, kmp_int32 gtid);

extern "C" void __kmpc_barrier(ident_t *loc, kmp_int32 global_tid);

extern "C" void __kmpc_for_static_init_4( ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter,
                          kmp_int32 *plower, kmp_int32 *pupper,
                          kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk );

extern "C" void __kmpc_for_static_fini( ident_t *loc, kmp_int32 global_tid );

extern "C" int __kmpc_single(ident_t *loc, int tid);
extern "C" void __kmpc_end_single(ident_t *loc, int tid);
