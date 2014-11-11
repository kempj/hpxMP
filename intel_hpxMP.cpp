#include "intel_hpxMP.h"
#include "loop_schedule.h"
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <assert.h>

using std::cout;
using std::endl;

boost::shared_ptr<hpx_runtime> hpx_backend;

//typedef void (*omp_micro)(int , frame_pointer_t);
//typedef void (*microtask_t)( int *gtid, int *npr, ... );
//typedef void (*kmpc_micro)  ( kmp_int32 * global_tid, kmp_int32 * bound_tid, ... );

bool in_parallel = false;

struct task_args {
    kmpc_micro fork_func;
    int argc;
    void **argv;
};

void start_backend(){
    if( !hpx::get_runtime_ptr() ) {
        hpx_backend.reset(new hpx_runtime());
    }
}

//extern int __kmp_invoke_microtask( microtask_t pkfn, int gtid, int npr, int argc, void *argv[]);
void omp_thread_func(int tid, void *fp) {
    //int tid = hpx_backend->get_thread_num();//not sure if correct
    task_args *args = (task_args*)fp;
    void **argv = args->argv;
    int argc = args->argc - 1;
    //Are the arguments packed in order, or in reverse order?
    //They are in reverse order, and argv points one past the data.

    assert(argc < 2);
    switch(argc) {
        case 0: args->fork_func(&tid, &tid);
                break;
        case 1: args->fork_func(&tid, &tid, argv[-argc-1]);
                break;
                /*
        case 2: args->fork_func(&tid, &tid, args->argv[0], args->argv[1]);
                break;
        case 3: args->fork_func(&tid, &tid, args->argv[0], args->argv[1], args->argv[2]);
                break;
        case 4: args->fork_func(&tid, &tid, args->argv[0], args->argv[1], args->argv[2],
                          args->argv[3]);
                break;
        case 5: args->fork_func(&tid, &tid, args->argv[0], args->argv[1], args->argv[2],
                          args->argv[3], args->argv[4]);
                break;
                */
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
    void **argp = argv;

    va_list     ap;
    va_start(   ap, microtask );

    for( int i = argc-1; i >= 0; --i ){
        *argv++ = va_arg( ap, void * );
    }
    va_end( ap );
    args.argc = argc;
    args.argv = argv;

    args.fork_func = microtask;

    if( hpx::threads::get_self_ptr() ) {
        hpx_backend->fork(0, omp_thread_func, (void*)&args);
    } else {
        in_parallel = true;
        hpx_backend->fork(0, omp_thread_func, (void*)&args);
        in_parallel = false;
    }
    delete[] argp;
}

void
__kmpc_for_static_init_4( ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter,
                          kmp_int32 *plower, kmp_int32 *pupper,
                          kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk ){
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
    hpx_backend->set_num_threads(num_threads);
}

void
__kmpc_barrier(ident_t *loc, kmp_int32 global_tid) {
    hpx_backend->barrier_wait();
}

int  __kmpc_cancel_barrier(ident_t* loc_ref, kmp_int32 gtid){
    hpx_backend->barrier_wait();
    return 0;
}

int __kmpc_global_thread_num(ident_t *loc){
    return 0;
}

int __kmpc_single(ident_t *loc, int tid){
    if(!in_parallel)
        return 1;
    parallel_region *team = hpx_backend->get_team();

    if(0 == team->single_counter++){
        return 1;
    }
    return 0;
}

void __kmpc_end_single(ident_t *loc, int tid){
    parallel_region *team = hpx_backend->get_team();
    if(!in_parallel)
        return;
    team->single_counter--;
}

int __kmpc_master(ident_t *loc, int global_tid){
    //TODO: if master can be called from tasks, then this doesn't work.
    if(hpx_backend->get_thread_num() == 0) 
        return 1;
    return 0;
}

void __kmpc_end_master(ident_t *loc, int global_tid){
}

void
__kmpc_critical( ident_t * loc, kmp_int32 global_tid, kmp_critical_name * crit ) {
    //I am not sure how the crit name is allocated or initialized.
    hpx_backend->crit_mtx.lock();
}

void
__kmpc_end_critical(ident_t *loc, kmp_int32 global_tid, kmp_critical_name *crit) {
    hpx_backend->crit_mtx.unlock();
}

void __kmpc_flush(ident_t *loc, ...){
    __sync_synchronize();
}

//I think I need to pair up *data to with the memory allocated to represend the threadlocal version
void* __kmpc_threadprivate_cached( ident_t *loc, kmp_int32 tid, void *data, size_t size, void ***cache){
    if(!hpx_backend) {
        start_backend();
    }
    if(!in_parallel)
        //special thread 0
    void ** tp_pointer = hpx_backend->get_threadprivate();
    if(tp_pointer){
        return *(tp_pointer);
    }
    void *tmp = new char[size];
    *(tp_pointer) = tmp;
    //std::copy((char*)data, (char*)data + size, (char*)tmp);
    return tmp;
}

//Library functions:--------------------------------------------------
int omp_get_thread_num(){
    //TODO: check if hpx is running
    return hpx_backend->get_thread_num();
}

int omp_get_num_threads(){
    if(in_parallel){
        return hpx_backend->get_team()->nthreads_var;
    } else {
        return 1;
    }
}

double omp_get_wtime(){
    if(!hpx_backend)
        start_backend();
    return hpx_backend->get_time();
}

double omp_get_wtick(){
    return .000000001;
}

int omp_in_parallel(){
    return in_parallel;
}

void omp_init_lock(omp_lock_t *lock){
    if(!hpx_backend) {
        start_backend();
    }
    lock = new omp_lock_t;
}

void omp_destroy_lock(omp_lock_t *lock) {
    delete lock;
}

