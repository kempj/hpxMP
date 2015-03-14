//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 8

#include "hpx_runtime.h"

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
    hpx::get_runtime().stop();
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
    //TODO:
    //OMP_NESTED -> initial_nest_var
    //cancel_var
    //stacksize_var
    /*
    char const* omp_max_levels = getenv("OMP_MAX_ACTIVE_LEVELS");
    if(omp_max_levels != NULL) { max_active_levels_var = atoi(omp_max_levels); }
    
    //Not device specific, so it needs to move to parallel region:
    char const* omp_thread_limit = getenv("OMP_THREAD_LIMIT");
    if(omp_thread_limit != NULL) { thread_limit_var = atoi(omp_thread_limit); }
    */

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

    if(external_hpx)
        return;


    //char const* omp_stack_size = getenv("OMP_STACKSIZE");
        
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

        // FIXME: For correctness check for signed overflow.
        argc = hpx_args.size() + 1;
        argv = new char*[argc];

        // FIXME: Should we do escaping?    
        for (boost::uint64_t i = 0; i < hpx_args.size(); ++i) {
            argv[i + 1] = const_cast<char*>(hpx_args[i].c_str());
        }
    } else {
        argc = 1;
        argv = new char*[argc];
    }
    argv[0] = const_cast<char*>("hpxMP");


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


#ifdef BUILD_UH
//This needs to be here to make sure to wait for dependant children
// finish before destroying the stack of the task. If this work is 
// done in task_create the stack of the user's task does not get
// preserved get Note: in OpenUH this gets called at the end of 
// implicit and explicit tasks 

void hpx_runtime::task_exit() {
    auto *task_data = get_task_data();
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(task_data->thread_mutex);
        while(task_data->blocking_children > 0) {
            task_data->thread_cond.wait(lock);
        }
    }
}

void task_setup( omp_task_func task_func, void *fp, void *firstprivates,
                 omp_task_data *parent, int blocks_parent, omp_icv icv_vars, 
                 parallel_region *team, int thread_num ) {

    omp_task_data task_data(thread_num, team, icv_vars);
    set_thread_data( get_self_id(), reinterpret_cast<size_t>(&task_data));

    task_func(firstprivates, fp);

    if(blocks_parent) {
        parent->blocking_children--;
        if(parent->blocking_children == 0) {
            parent->thread_cond.notify_one();
        }
    }
    task_data.team->num_tasks--;
    if(task_data.team->num_tasks == 0) {
        task_data.team->cond.notify_all();
    }
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied,
                               int blocks_parent ) {
    auto *parent = get_task_data();
    parent->team->num_tasks++;
    if(blocks_parent) {
        parent->blocking_children += 1;
        parent->has_dependents = true;
    }
    parent->task_handles.push_back(
            hpx::async( task_setup, taskfunc, frame_pointer, firstprivates, parent, 
                        blocks_parent, parent->icv, parent->team, parent->thread_num));
}

#endif
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

//According to the spec, this should only be called from a "thread", 
// and never from inside an openmp tasks.
void hpx_runtime::barrier_wait(){
    auto *team = get_team();
    while(team->num_tasks > team->num_threads){
        hpx::this_thread::yield();
    }
    if(team->num_threads > 1) {
        team->globalBarrier.wait();
    }
}

int hpx_runtime::get_thread_num() {
    auto *data = get_task_data();
    return get_task_data()->thread_num;
}

void hpx_runtime::task_wait() {
    auto *tasks = &(get_task_data()->task_handles);
    hpx::wait_all(*tasks);
    tasks->clear();
}

void intel_task_setup( kmp_routine_entry_t task_func, int gtid, void *task,
                       omp_icv icv_vars,
                       parallel_region *team, int thread_num) {

    omp_task_data task_data(gtid, team, icv_vars);
    set_thread_data( get_self_id(), reinterpret_cast<size_t>(&task_data));

    task_func(gtid, task);

    team->num_tasks--;
    if(team->num_tasks == 0) {
        team->cond.notify_all();
    }
    delete[] (char*)task;
}

void df_wrapper_func(kmp_task_t *thunk, int gtid){//, vector<future<void>> dep_futures) {
}

void hpx_runtime::create_df_task( kmp_task_t *thunk, int gtid, vector<int64_t> in_deps, vector<int64_t> out_deps) {
    auto task = get_task_data();
    vector<shared_future<void>> dep_futures;

    for(int i = 0; i < in_deps.size(); i++) {
        if(task->df_map.count( in_deps[i]) > 0) {
            dep_futures.push_back(task->df_map[i]);
        }
    }
    for(int i = 0; i < in_deps.size(); i++) {
        if(task->df_map.count( in_deps[i]) > 0) {
            dep_futures.push_back(task->df_map[i]);
        }
    }

    auto wrapped_routine = hpx::util::unwrapped(df_wrapper_func);
    shared_future<kmp_task_t*> futurized_task_data = hpx::make_ready_future(thunk);
    shared_future<int> futurized_gtid = hpx::make_ready_future(gtid);

    auto current_task = dataflow( wrapped_routine, futurized_task_data, futurized_gtid//, hpx::when_all(dep_futures));

    //Then add out deps to the map
    /*
    for(int i = 0 ; i < ndeps; i++) {
        if(dep_list[i].flags.out) {
            df_map->at(dep_list[i].base_addr) = dep_future;
        }
    }
    */
}

void hpx_runtime::create_intel_task( kmp_routine_entry_t task_func, int gtid, void *task){
    auto *current_task = get_task_data();
    current_task->team->num_tasks++;
    current_task->task_handles.push_back( 
                    hpx::async( intel_task_setup, task_func, gtid, task, current_task->icv,
                                current_task->team, current_task->thread_num));
}


#ifdef BUILD_UH
void thread_setup( omp_micro thread_func, void *fp, int tid,
                   parallel_region *team, omp_task_data *parent ) {
#else
void thread_setup( invoke_func kmp_invoke, microtask_t thread_func, 
                   int argc, void **argv, int tid,
                   parallel_region *team, omp_task_data *parent ) {
#endif
    
    omp_task_data task_data(tid, team, parent);
    auto thread_id = get_self_id();
    set_thread_data( thread_id, reinterpret_cast<size_t>(&task_data));

#ifdef BUILD_UH
    thread_func(tid, fp);
#else
    //Not sure why I need to do this, but the asm segfaults if I don't
    //It seems to be fine fo argc > 0.
    if(argc == 0) {
        thread_func(&tid, &tid);
    } else {
        kmp_invoke(thread_func, tid, tid, argc, argv);
    }
#endif

    //TODO: This should wait on the number of tasks the current
    // "thread"/task has outstanding. This would be an optimization, 
    // as it is currently correct.
    team->num_tasks--;
    if(team->num_tasks == 0) {
        team->cond.notify_all();
    }
}

//This is the only place where I can't call get_thread.
//That data is not initialized for the new hpx threads yet.
#ifdef BUILD_UH
void fork_worker( omp_micro thread_func, frame_pointer_t fp,
                  omp_task_data *parent) {
#else
void fork_worker( invoke_func kmp_invoke, microtask_t thread_func,
                  int argc, void **argv,
                  omp_task_data *parent) {
#endif

    parallel_region team(parent->team, parent->threads_requested);
    vector<hpx::lcos::future<void>> threads;

    for( int i = 0; i < parent->threads_requested; i++ ) {
        team.num_tasks++;
#ifdef BUILD_UH
        threads.push_back( hpx::async( thread_setup, *thread_func, fp, i, &team, parent ) );
#else
        threads.push_back( hpx::async( thread_setup, kmp_invoke, thread_func, argc, argv, i, &team, parent ) );
#endif
    }

    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(team.thread_mtx);
        while(team.num_tasks > 0) {
            team.cond.wait(lock);
        }
    }
    hpx::wait_all(threads);

    /*
    for(int i = 0; i < parent->threads_requested; i++) {
        std::string name = "/threads{locality#0/worker-thread#";
        name += std::to_string(i);
        name += "}/count/cumulative";
        hpx::performance_counters::performance_counter completed(name);
        //hpx::performance_counters::performance_counter
        //completed(
        //"/threads{locality#0/total}/count/cumulative");
        int Ncompleted = completed.get_value<int>().get();
        cout << Ncompleted << "completed hpx threads on thread " << i << endl;
    }*/
}

#ifdef BUILD_UH
void fork_and_sync( omp_micro thread_func, frame_pointer_t fp, 
                    omp_task_data *parent, boost::mutex& mtx, 
                    boost::condition& cond, bool& running ) {
    fork_worker(thread_func, fp, parent);
#else
void fork_and_sync( invoke_func kmp_invoke, microtask_t thread_func, 
                    int argc, void **argv,
                    omp_task_data *parent, boost::mutex& mtx, 
                    boost::condition& cond, bool& running ) {
    fork_worker(kmp_invoke, thread_func, argc, argv, parent);
#endif

    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}
 
//For Intel, the Nthreads isn't passed in, another function sets Nthreads, so Nthreads should be 0;
// Also for Intel, fp is not a frame pointer, but a pointer to a struct,
//TODO: according to the spec, the current thread should be thread 0 of the new team, and execute the new work.
#ifdef BUILD_UH
void hpx_runtime::fork(int Nthreads, omp_micro thread_func, frame_pointer_t fp)
{ 
    omp_task_data *current_task = get_task_data();
    current_task->set_threads_requested( Nthreads );
    if( hpx::threads::get_self_ptr() ) {
        fork_worker(thread_func, fp, current_task);
#else
void hpx_runtime::fork(invoke_func kmp_invoke, microtask_t thread_func, int argc, void** argv)
{ 
    omp_task_data *current_task = get_task_data();
    if( hpx::threads::get_self_ptr() ) {
        fork_worker(kmp_invoke, thread_func, argc, argv, current_task);
#endif
    } else {
        boost::mutex mtx;
        boost::condition cond;
        bool running = false;
        hpx::applier::register_thread_nullary(
                std::bind(&fork_and_sync,
#ifdef BUILD_UH
                    thread_func, fp, 
#else
                    kmp_invoke, thread_func, argc, argv,
#endif
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

