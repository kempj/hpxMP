#include <cstdint>
#include "intel_hpxMP.h"


extern "C" void __kmpc_for_static_fini( ident_t *loc, kmp_int32 global_tid );

//TODO:
//__kmpc_for_static_init_8

extern "C" void 
__kmpc_for_static_init_4( ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, 
                          kmp_int32 *plastiter,kmp_int32 *plower, kmp_int32 *pupper, 
                          kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk );

extern "C" void 
__kmpc_for_static_init_4u( ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype,
                           kmp_int32 *plastiter, uint32_t *plower, uint32_t *pupper,
                           kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk );

extern "C" void
__kmpc_dispatch_init_4( ident_t *loc, kmp_int32 gtid, enum sched_type schedule,
                        kmp_int32 lb, kmp_int32 ub, kmp_int32 st, kmp_int32 chunk );
