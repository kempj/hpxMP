#include <cstdint>
#include "intel_hpxMP.h"


extern "C" void __kmpc_for_static_fini( ident_t *loc, int32_t global_tid );

//TODO:
//__kmpc_for_static_init_8

extern "C" void 
__kmpc_for_static_init_4( ident_t *loc, int32_t gtid, int32_t schedtype, 
                          int32_t *p_last_iter,int32_t *p_lower, int32_t *p_upper, 
                          int32_t *p_stride, int32_t incr, int32_t chunk );

extern "C" void 
__kmpc_for_static_init_4u( ident_t *loc, int32_t gtid, int32_t schedtype,
                           int32_t *p_last_iter, uint32_t *p_lower, uint32_t *p_upper,
                           int32_t *p_stride, int32_t incr, int32_t chunk );
extern "C" void 
__kmpc_for_static_init_8( ident_t *loc, int32_t gtid, 
                          int32_t schedtype, int32_t *p_last_iter, 
                          int64_t *p_lower, int64_t *p_upper, 
                          int64_t *p_stride, int64_t incr, int64_t chunk );

extern "C" void 
__kmpc_for_static_init_8u( ident_t *loc, int32_t gtid, 
                           int32_t schedtype, int32_t *p_last_iter, 
                           uint64_t *p_lower, uint64_t *p_upper,
                           int64_t *p_stride, int64_t incr, int64_t chunk );



extern "C" void
__kmpc_dispatch_init_4( ident_t *loc, int32_t gtid, enum sched_type schedule,
                        int32_t lb, int32_t ub,
                        int32_t st, int32_t chunk );

extern "C" void
__kmpc_dispatch_init_4u( ident_t *loc, int32_t gtid, enum sched_type schedule,
                         uint32_t lb, uint32_t ub, 
                         int32_t st, int32_t chunk );

extern "C" void
__kmpc_dispatch_init_8( ident_t *loc, int32_t gtid, enum sched_type schedule,
                        int64_t lb, int64_t ub, 
                        int64_t st, int64_t chunk );

extern "C" void
__kmpc_dispatch_init_8u( ident_t *loc, int32_t gtid, enum sched_type schedule,
                         uint64_t lb, uint64_t ub, 
                         int64_t st, int64_t chunk );

extern "C" int
__kmpc_dispatch_next_4( ident_t *loc, int32_t gtid, int32_t *p_last,
                        int32_t *p_lb, int32_t *p_ub, int32_t *p_st );

