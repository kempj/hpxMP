//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define  HPX_LIMIT 9
#include "hpx_runtime.h"

using std::cout;
using std::endl;

using hpx::lcos::local::dataflow;
using hpx::util::unwrapped;
using hpx::make_ready_future;
using hpx::threads::set_thread_data;
using hpx::threads::get_thread_data;
using hpx::threads::get_self_id;


extern boost::shared_ptr<hpx_runtime> hpx_backend;


void wait_for_startup(boost::mutex& mtx, boost::condition& cond, bool& running){
    cout << "HPX OpenMP runtime has started" << endl;
    {   // Let the main thread know that we're done.
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}

void fini_runtime() {
    cout << "Stopping HPX OpenMP runtime" << endl;
    //this should only be done if this runtime started hpx
    hpx::get_runtime().stop();
}

void start_hpx(int initial_num_threads) {
#ifdef OMP_COMPLIANT
    int num_hard_coded_args = 2;
#else
    int num_hard_coded_args = 1;
#endif
    std::vector<std::string> cfg;
    int argc;
    char ** argv;
    using namespace boost::assign;
    cfg += "hpx.os_threads=" + boost::lexical_cast<std::string>(initial_num_threads);
    cfg += "hpx.stacks.use_guard_pages=0";
    cfg += "hpx.run_hpx_main!=0";
    //cfg += "hpx.stacks.huge_size=0x2000000";

    char const* hpx_args_raw = getenv("OMP_HPX_ARGS");

    std::vector<std::string> hpx_args;

    if (hpx_args_raw) { 
        std::string tmp(hpx_args_raw);

        boost::algorithm::split(hpx_args, tmp,
            boost::algorithm::is_any_of(";"),
                boost::algorithm::token_compress_on);

        argc = hpx_args.size() + num_hard_coded_args;
        argv = new char*[argc];

        for (boost::uint64_t i = 0; i < hpx_args.size(); ++i) {
            argv[i + num_hard_coded_args] = const_cast<char*>(hpx_args[i].c_str());
        }
    } else {
        argc = num_hard_coded_args;
        argv = new char*[argc];
    }
    argv[0] = const_cast<char*>("hpxMP");
#ifdef OMP_COMPLIANT
    argv[1] = const_cast<char*>("--hpx:queuing=static");
#endif
    hpx::util::function_nonser<int(boost::program_options::variables_map& vm)> f;
    boost::program_options::options_description desc_cmdline; 

    boost::mutex local_mtx;
    boost::condition cond;//TODO: replace this with something that can be checked later, once hpx is needed.
    bool running = false;

    hpx::start(f, desc_cmdline, argc, argv, cfg,
            std::bind(&wait_for_startup, boost::ref(local_mtx), boost::ref(cond), boost::ref(running)));

    { 
        boost::mutex::scoped_lock lk(local_mtx);
        if (!running)
            cond.wait(lk);
    }
    
    atexit(fini_runtime);

    delete[] argv;
}

hpx_runtime::hpx_runtime() {
    int initial_num_threads;
    num_procs = hpx::threads::hardware_concurrency();
    char const* omp_num_threads = getenv("OMP_NUM_THREADS");

    if(omp_num_threads != NULL){
        initial_num_threads = atoi(omp_num_threads);
    } else { 
        initial_num_threads = num_procs;
    }
    //char const* omp_stack_size = getenv("OMP_STACKSIZE");
    //OMP_NESTED -> initial_nest_var
    //cancel_var
    //stacksize_var
    //char const* omp_max_levels = getenv("OMP_MAX_ACTIVE_LEVELS");
    //if(omp_max_levels != NULL) { max_active_levels_var = atoi(omp_max_levels); }
    ////Not device specific, so it needs to move to parallel region:
    //char const* omp_thread_limit = getenv("OMP_THREAD_LIMIT");
    //if(omp_thread_limit != NULL) { thread_limit_var = atoi(omp_thread_limit); }

    external_hpx = hpx::get_runtime_ptr();
    if(external_hpx){
        //It doesn't make much sense to try and use openMP thread settings
        // when the application has already initialized it's own threads.
        num_procs = hpx::get_os_thread_count();
        initial_num_threads = num_procs;
    }

    //TODO: nthreads_var is a list of ints where the nth item corresponds
    // to the number of threads in nth level parallel regions.

    implicit_region.reset(new parallel_region(1));
    initial_thread.reset(new omp_task_data(implicit_region.get(), &device_icv, initial_num_threads));
    walltime.reset(new high_resolution_timer);

    if(!external_hpx) {
        start_hpx(initial_num_threads);
    }
}

//This isn't really a thread team, it's a region. I think.
parallel_region* hpx_runtime::get_team(){
    auto task_data = get_task_data();
    auto team = task_data->team;
    return team;
}

omp_task_data* hpx_runtime::get_task_data(){
    omp_task_data *data;
    if(hpx::threads::get_self_ptr()) {
        data = reinterpret_cast<omp_task_data*>(get_thread_data(get_self_id()));
        if(!data) {
            data = initial_thread.get();
        }
    } else { 
        data = initial_thread.get();
    }
    return data;
}

double hpx_runtime::get_time() {
    return walltime->now();
}

int hpx_runtime::get_num_threads() {
    return get_team()->num_threads;
}

int hpx_runtime::get_num_procs() {
    return num_procs;
}

void hpx_runtime::set_num_threads(int nthreads) {
    if(nthreads > 0) {
        get_task_data()->icv.nthreads = nthreads;
        get_task_data()->threads_requested = nthreads;
    }
}

int hpx_runtime::get_thread_num() {
#ifdef OMP_COMPLIANT
    return hpx::get_worker_thread_num();
#else
    return get_task_data()->local_thread_num;
#endif
}

// this should only be called from implicit tasks
void hpx_runtime::barrier_wait(){
    auto *team = get_team();
    task_wait();
#ifdef OMP_COMPLIANT
    while(team->exec->num_pending_closures() > 0 ) {
        hpx::this_thread::yield();
    }
#else
    while(team->num_tasks > 0) {
        hpx::this_thread::yield();
    }
#endif
    if(team->num_threads > 1) {
        team->globalBarrier.wait();
    }
}

bool hpx_runtime::start_taskgroup(){
    auto *task = get_task_data();
    task->in_taskgroup = true;
#ifdef OMP_COMPLIANT
    task->tg_exec.reset(new local_priority_queue_executor(task->local_thread_num));
#endif
    return true;
}

void hpx_runtime::end_taskgroup() {
    auto *task = get_task_data();
#ifdef OMP_COMPLIANT
    //FIXME: This currently doesn't wait on dataflow tasks.
    task->tg_exec.reset();
#endif
    task->in_taskgroup = false;
}

void hpx_runtime::task_wait() {
    auto *task = get_task_data();
    if(task->df_map.size() > 0) {
        task->last_df_task.wait();
    }
    while( *(task->num_child_tasks) > 0 ) {
        hpx::this_thread::yield();
    }
}

// container_task_counter can be either num_taskgroup_tasks or num_thread_tasks
void task_setup( int gtid, kmp_task_t *task, omp_icv icv, 
                 shared_ptr<atomic<int64_t>> parent_task_counter,
                 parallel_region *team) {

    auto task_func = task->routine;
    omp_task_data task_data(gtid, team, icv);
    set_thread_data( get_self_id(), reinterpret_cast<size_t>(&task_data));

    task_func(gtid, task);

    *(parent_task_counter) -= 1;
#ifndef OMP_COMPLIANT
    team->num_tasks--;
#endif

    delete[] (char*)task;
}

void tg_task_setup( int gtid, kmp_task_t *task, omp_icv icv, 
                 shared_ptr<local_priority_queue_executor> tg_exec,
                 parallel_region *team) {
    auto task_func = task->routine;
    omp_task_data task_data(gtid, team, icv);
    task_data.in_taskgroup = true;
    set_thread_data( get_self_id(), reinterpret_cast<size_t>(&task_data));

    task_func(gtid, task);

    delete[] (char*)task;
}

//shared_ptr is used for these counters, because the parent/calling task may terminate at any time,
//causing its omp_task_data to be deallocated.
void hpx_runtime::create_task( kmp_routine_entry_t task_func, int gtid, kmp_task_t *thunk){
    auto *current_task = get_task_data();

    if(current_task->team->num_threads > 1) {
#ifdef OMP_COMPLIANT
        if(current_task->in_taskgroup) { 
            hpx::apply( *(current_task->tg_exec), tg_task_setup, gtid, thunk, current_task->icv,
                        current_task->tg_exec, current_task->team );
        } else {
            *(current_task->num_child_tasks) += 1;
            hpx::apply( *(current_task->team->exec), task_setup, gtid, thunk, current_task->icv,
                        current_task->num_child_tasks, current_task->team );
        }
#else
        //TODO: add taskgroups in non compliant version
        current_task->team->num_tasks++;
        hpx::apply(task_setup, gtid, thunk, current_task->icv,
                    current_task->num_child_tasks, current_task->team );
#endif
    } else {
        *(current_task->num_child_tasks) += 1;
        task_setup(gtid, thunk, current_task->icv, current_task->num_child_tasks, current_task->team);
    }
}

void df_task_wrapper( int gtid, kmp_task_t *task, omp_icv icv, 
                      shared_ptr<atomic<int64_t>> task_counter,
                      parallel_region *team, 
                      vector<shared_future<void>> deps) 
{
    task_setup( gtid, task, icv, task_counter, team);
}

void df_tg_task_wrapper( int gtid, kmp_task_t *task, omp_icv icv, 
                        shared_ptr<local_priority_queue_executor> tg_exec,
                        parallel_region *team, 
                        vector<shared_future<void>> deps) 
{
    tg_task_setup( gtid, task, icv, tg_exec, team);
}

//The input on the Intel call is a pair of pointers to arrays of dep structs, and the length of
//these arrays.
//The structs contain a pointer and a flag for in or out dep
//I need dep_futures vector, so I can do a wait_all on it.
void hpx_runtime::create_df_task( int gtid, kmp_task_t *thunk, vector<int64_t> in_deps, vector<int64_t> out_deps) {
    auto task = get_task_data();
    auto team = task->team;
    if(team->num_threads == 1 ) {
        create_task(thunk->routine, gtid, thunk);
    }
    vector<shared_future<void>> dep_futures;
    dep_futures.reserve( in_deps.size() + out_deps.size() );

    //Populating a vector of futures that the task depends on
    for(int i = 0; i < in_deps.size(); i++) {
        if(task->df_map.count( in_deps[i] ) > 0) {
            dep_futures.push_back(task->df_map[in_deps[i]]);
        }
    }
    for(int i = 0; i < out_deps.size(); i++) {
        if(task->df_map.count( out_deps[i] ) > 0) {
            dep_futures.push_back(task->df_map[out_deps[i]]);
        }
    }

    shared_future<void> new_task;

    if(task->in_taskgroup) {
    } else {
        *(task->num_child_tasks) += 1;
    }
#ifndef OMP_COMPLIANT
    team->num_tasks++;
#endif
    if(dep_futures.size() == 0) {
#ifdef OMP_COMPLIANT
        if(task->in_taskgroup) {
            new_task = hpx::async( *(task->tg_exec), tg_task_setup, gtid, thunk, task->icv,
                                    task->tg_exec, team);
        } else {
            new_task = hpx::async( *(team->exec), task_setup, gtid, thunk, task->icv,
                                    task->num_child_tasks, team);
        }
#else
        new_task = hpx::async( task_setup, gtid, thunk, task->icv,
                                task->num_child_tasks, team);
#endif
    } else {
        shared_future<kmp_task_t*>      f_thunk = make_ready_future( thunk );
        shared_future<int>              f_gtid  = make_ready_future( gtid );
        shared_future<omp_icv>          f_icv   = make_ready_future( task->icv );
        shared_future<parallel_region*> f_team  = make_ready_future( team );
        shared_future<shared_ptr<atomic<int64_t>>> f_parent_counter  = hpx::make_ready_future( task->num_child_tasks);
        shared_future<shared_ptr<atomic<int64_t>>> f_counter;


#ifdef OMP_COMPLIANT
        //if(task->in_taskgroup) {
            //FIXME: This isn't being added to the taskgroup executor.
        //    new_task = dataflow( unwrapped(df_tg_task_wrapper), f_gtid, f_thunk, f_icv, 
        //                         make_ready_future(task->tg_exec),
        //                         f_team, hpx::when_all(dep_futures) );
        //} else {
            new_task = dataflow( unwrapped(df_task_wrapper), f_gtid, f_thunk, f_icv, 
                                 f_parent_counter, 
                                 f_team, hpx::when_all(dep_futures) );
        //}
#else
        new_task = dataflow( unwrapped(df_task_wrapper), f_gtid, f_thunk, f_icv, 
                             f_parent_counter, 
                             f_team, hpx::when_all(dep_futures) );
#endif
    }
    for(int i = 0 ; i < out_deps.size(); i++) {
        task->df_map[out_deps[i]] = new_task;
    }
    task->last_df_task = new_task;
}

void thread_setup( invoke_func kmp_invoke, microtask_t thread_func, 
                   int argc, void **argv, int tid,
                   parallel_region *team, omp_task_data *parent,
                   mutex_type& mtx,
                   hpx::lcos::local::condition_variable& cond,
                   atomic<int>& running_threads
                   ) {
    omp_task_data task_data(tid, team, parent);

    set_thread_data( get_self_id(), reinterpret_cast<size_t>(&task_data));

    if(argc == 0) { //note: kmp_invoke segfaults iff argc == 0
        thread_func(&tid, &tid);
    } else {
        kmp_invoke(thread_func, tid, tid, argc, argv);
    }
    while (*(task_data.num_child_tasks) > 0 ) {
        hpx::this_thread::yield();
    }

    if(--running_threads == 0) {
        hpx::lcos::local::spinlock::scoped_lock lk(mtx);
        cond.notify_all();
    }
}

//This is the only place where I can't call get_thread.
//That data is not initialized for the new hpx threads yet.
void fork_worker( invoke_func kmp_invoke, microtask_t thread_func,
                  int argc, void **argv,
                  omp_task_data *parent) 
{
    parallel_region team(parent->team, parent->threads_requested);
    
#ifdef OMP_COMPLIANT
    team.exec.reset(new local_priority_queue_executor(parent->threads_requested));
#endif

    hpx::lcos::local::condition_variable cond;
    mutex_type mtx;
    atomic<int> running_threads;
    running_threads = parent->threads_requested;

    for( int i = 0; i < parent->threads_requested; i++ ) {
        hpx::applier::register_thread_nullary(
                std::bind( &thread_setup, kmp_invoke, thread_func, argc, argv, i, &team, parent, 
                           boost::ref(mtx), boost::ref(cond), boost::ref(running_threads) ),
                "omp_implicit_task", hpx::threads::pending,
                true, hpx::threads::thread_priority_normal, i );
    }
    {
        hpx::lcos::local::spinlock::scoped_lock lk(mtx);
        while( running_threads > 0 ) {
            cond.wait(lk);
        }
    }
#ifndef OMP_COMPLIANT
    while(team.num_tasks > 0) {
        hpx::this_thread::yield();
    }
#endif
}

void fork_and_sync( invoke_func kmp_invoke, microtask_t thread_func, 
                    int argc, void **argv,
                    omp_task_data *parent, boost::mutex& mtx, 
                    boost::condition& cond, bool& running ) 
{
    fork_worker(kmp_invoke, thread_func, argc, argv, parent);
    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}
 
//TODO: This can make main an HPX high priority thread
//TODO: according to the spec, the current thread should be thread 0 of the new team, and execute the new work.
void hpx_runtime::fork(invoke_func kmp_invoke, microtask_t thread_func, int argc, void** argv)
{ 
    omp_task_data *current_task = get_task_data();
    if( hpx::threads::get_self_ptr() ) {
        fork_worker(kmp_invoke, thread_func, argc, argv, current_task);
    } else {
        boost::mutex mtx;
        boost::condition cond;
        bool running = false;
        hpx::applier::register_thread_nullary(
                std::bind(&fork_and_sync,
                    kmp_invoke, thread_func, argc, argv,
                    current_task, boost::ref(mtx), boost::ref(cond), boost::ref(running))
                , "ompc_fork_worker");
        {   // Wait for the thread to run.
            boost::mutex::scoped_lock lk(mtx);
            while (!running)
                cond.wait(lk);
        }
    }
    current_task->set_threads_requested(current_task->icv.nthreads );
}

