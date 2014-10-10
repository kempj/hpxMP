#include "hpx_runtime.h"
#include <cstdarg>

typedef int kmp_int32;
typedef long long kmp_int64;

typedef void                (*microtask_t)( int *gtid, int *npr, ... );
typedef int                 (*launch_t)( int gtid );

typedef void (*kmpc_micro)  ( kmp_int32 * global_tid, kmp_int32 * bound_tid, ... );


typedef struct ident {
    kmp_int32 reserved_1;   /**<  might be used in Fortran; see above  */
    kmp_int32 flags;        /**<  also f.flags; KMP_IDENT_xxx flags; KMP_IDENT_KMPC identifies this union member  */
    kmp_int32 reserved_2;   /**<  not really used in Fortran any more; see above */
    kmp_int32 reserved_3;   /**< source[4] in Fortran, do not use for C++  */
    char const *psource;    /**< String describing the source location.*/
} ident_t;

extern "C" void   __kmpc_fork_call          ( ident_t *, kmp_int32 nargs, kmpc_micro microtask, ... );
extern "C" int  __kmpc_global_thread_num(ident_t *loc);
extern "C" void __kmpc_push_num_threads ( ident_t *loc, kmp_int32 global_tid, kmp_int32 num_threads );
extern "C" int  __kmpc_cancel_barrier(ident_t* loc_ref, kmp_int32 gtid);


/*
extern int __kmp_fork_call( ident_t *loc, int gtid, int exec_master,
                            kmp_int32 argc, microtask_t microtask, 
                            launch_t invoker,
#if (KMP_ARCH_ARM || KMP_ARCH_X86_64) && KMP_OS_LINUX
                            va_list *ap
#else
                            va_list ap
#endif
        );
        */
