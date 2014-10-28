#include "intel_hpxMP.h"
#include "loop_data.h"
#include "loop_schedule.h"
#include <boost/shared_ptr.hpp>
#include <iostream>

using std::cout;
using std::endl;

boost::shared_ptr<hpx_runtime> hpx_backend;
boost::shared_ptr<loop_data> loop_sched;

//typedef void (*omp_micro)(int , frame_pointer_t);
//typedef void (*microtask_t)( int *gtid, int *npr, ... );
//typedef void (*kmpc_micro)  ( kmp_int32 * global_tid, kmp_int32 * bound_tid, ... );

//bool started = false;
//kmpc_micro fork_func = 0;

struct task_args {
    int argc;
    void **argv;
};

void start_backend(){
    if( !hpx::get_runtime_ptr() ) {
        hpx_backend.reset(new hpx_runtime());
        loop_sched.reset(new loop_data());
    }
}

//extern int __kmp_invoke_microtask( microtask_t pkfn, int gtid, int npr, int argc, void *argv[]);
void omp_thread_func(void *firstprivates, void *fp) {
    int tid = hpx_backend->get_thread_num();//not sure if correct
    task_args *args = (task_args*)fp;

    switch(args->argc) {
        case 0: fork_func(&tid, &tid);
                break;
        case 1: fork_func(&tid, &tid, args->argv[0]);
                break;
        case 2: fork_func(&tid, &tid, args->argv[0], args->argv[1]);
                break;
        case 3: fork_func(&tid, &tid, args->argv[0], args->argv[1], args->argv[2]);
                break;
        case 4: fork_func(&tid, &tid, args->argv[0], args->argv[1], args->argv[2],
                          args->argv[3]);
                break;
        case 5: fork_func(&tid, &tid, args->argv[0], args->argv[1], args->argv[2],
                          args->argv[3], args->argv[4]);
                break;
    }
}

void
__kmpc_fork_call(ident_t *loc, kmp_int32 argc, kmpc_micro microtask, ...)
{
    task_args args;
    if(!hpx_backend) {
        start_backend();
    }

    void **argv = new void*[argc];

    va_list     ap;
    va_start(   ap, microtask );

    for( int i = argc-1; i >= 0; --i ){
        *argv++ = va_arg( ap, void * );
    }
    va_end( ap );
    args.argc = argc;
    args.argv = argv;

    fork_func = microtask;

    started = true;
    hpx_backend->fork(0, omp_thread_func, (void*)&args);
    started = false;
    
    delete[] argv;
}

void
__kmpc_for_static_init_4( ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter,
                          kmp_int32 *plower, kmp_int32 *pupper,
                          kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk ){
    cout << "gtid = " << gtid << endl;
    __ompc_static_init_4(gtid, (omp_sched_t)2, plower, pupper, pstride, incr, chunk);
}

void
__kmpc_for_static_fini( ident_t *loc, kmp_int32 global_tid ){
    //Only seems to do internal tracking in intel runtime
}

void 
__kmpc_push_num_threads( ident_t *loc, 
                         kmp_int32 global_tid, 
                         kmp_int32 num_threads ){
}

void
__kmpc_barrier(ident_t *loc, kmp_int32 global_tid) {
    cout << "barrier, gtid = " << global_tid << endl;
    hpx_backend->barrier_wait();
}

int  __kmpc_cancel_barrier(ident_t* loc_ref, kmp_int32 gtid){
    return 0;
}

int __kmpc_global_thread_num(ident_t *loc){
    return 0;
}

