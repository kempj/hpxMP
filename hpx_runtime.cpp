//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 7

#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;


//atomic<int> num_tasks{0};
//boost::shared_ptr<hpx::lcos::local::condition_variable> thread_cond;

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

//This will break if the users shuts down hpx and an OpenMP call is made
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
    implicit_region.reset(new parallel_region(initial_num_threads));
    walltime.reset(new high_resolution_timer);

    if(external_hpx)
        return;
        
    std::vector<std::string> cfg;
    int argc;
    char ** argv;
    using namespace boost::assign;
    cfg += "hpx.os_threads=" + boost::lexical_cast<std::string>(initial_num_threads);
    cfg += "hpx.stacks.use_guard_pages=0";
    cfg += "hpx.run_hpx_main!=0";

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


    HPX_STD_FUNCTION<int(boost::program_options::variables_map& vm)> f;
    boost::program_options::options_description desc_cmdline; 

    boost::mutex local_mtx;
    boost::condition cond;
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

void** hpx_runtime::get_threadprivate() {
    //need to use special omp_thread_data for initial thread. TODO
    auto *task_data = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()));
    return &(task_data->threadprivate);
}

parallel_region* hpx_runtime::get_team(){
    parallel_region *team;
    if(hpx::threads::get_self_ptr()) {
        team = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()))->team;
    } else {
        team = implicit_region.get();
    }
    return team;
}

double hpx_runtime::get_time() {
    return walltime->now();
}

int hpx_runtime::get_num_threads() {
    int num_threads;
    if( hpx::threads::get_self_ptr() ){
        num_threads = get_team()->nthreads_var;
    } else {
        num_threads = 1;
    }
    return num_threads;
}

int hpx_runtime::get_num_procs() {
    return num_procs;
}

void hpx_runtime::set_num_threads(int nthreads) {
    if( hpx::threads::get_self_ptr() ){
        if(nthreads > 0) {
            get_team()->nthreads_var = nthreads;
        }
    } else {//not an hpx thread
       //initial_num_threads = nthreads; 
       implicit_region->nthreads_var = nthreads; 
    }
}

//According to the spec, this should only be called from a "thread", 
// and never from inside an openmp tasks.
void hpx_runtime::barrier_wait(){
    //auto *task_data = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()));
    while(get_team()->num_tasks > get_team()->nthreads_var){
        hpx::this_thread::yield();
    }
    get_team()->globalBarrier.wait();
}

int hpx_runtime::get_thread_num() {
    if( !hpx::threads::get_self_ptr() )
        return 0;
    auto thread_id = get_self_id();
    auto *data = reinterpret_cast<omp_thread_data*>( get_thread_data(thread_id) );
    return data->thread_num;
}

void hpx_runtime::task_wait() {
    auto *data = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()));
    hpx::wait_all(data->task_handles);
    data->task_handles.clear();
}

//This needs to be here to make sure to wait for dependant children finish
// before destroying the stack of the task. If this work is done in task_create
// the stack of the user's task does not get preserved.
// Note: in OpenUH this gets called at the end of implicit and explicit tasks
void hpx_runtime::task_exit() {
    auto *task_data = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()));
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(task_data->thread_mutex);
        while(task_data->blocking_children > 0) {
            task_data->thread_cond.wait(lock);
        }
    }
}

//Maybe have team hand out task id's round robin?
void intel_task_setup( kmp_routine_entry_t task_func, int gtid, void *task,
                       omp_thread_data *parent, parallel_region *team, int thread_num) {
    omp_thread_data task_data(thread_num, team);
    task_data.parent = parent;
    set_thread_data( get_self_id(), reinterpret_cast<size_t>(&task_data));

    task_func(gtid, task);

    team->num_tasks--;
    if(team->num_tasks == 0) {
        team->cond.notify_all();
    }
    //kmp_task_t *task = (kmp_task_t*)new char[sizeof_kmp_task_t + sizeof_shareds];
    delete task;
}

void hpx_runtime::create_intel_task( kmp_routine_entry_t task_func, int gtid, void *task){
    auto *parent_task = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()));
    parent_task->team->num_tasks++;
    parent_task->task_handles.push_back( 
                    hpx::async( intel_task_setup, task_func, gtid, task, parent_task,
                                parent_task->team, parent_task->thread_num));
}

void task_setup( omp_task_func task_func, void *fp, void *firstprivates,
                 omp_thread_data *parent_task, parallel_region *team,
                 int thread_num, int blocks_parent) {
    auto thread_id = get_self_id();
    omp_thread_data task_data(thread_num, team);
    task_data.parent = parent_task;
    set_thread_data( thread_id, reinterpret_cast<size_t>(&task_data));

    task_func(firstprivates, fp);

    task_data.is_finished = true;    
    if(blocks_parent) {
        parent_task->blocking_children--;
        if(parent_task->blocking_children == 0) {
            parent_task->thread_cond.notify_one();
        }
    }
    task_data.team->num_tasks--;
    if(task_data.team->num_tasks == 0) {
        task_data.team->cond.notify_all();
    }
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied, 
                               int blocks_parent) {
    auto *parent_task = reinterpret_cast<omp_thread_data*>(get_thread_data(get_self_id()));
    parent_task->team->num_tasks++;
    if(blocks_parent) {
        parent_task->blocking_children += 1;
        parent_task->has_dependents = true;
    }
    parent_task->task_handles.push_back( 
                    hpx::async( task_setup, taskfunc, frame_pointer, 
                                firstprivates, parent_task, parent_task->team,
                                parent_task->thread_num, blocks_parent));
}

//Thread tasks currently have no parent. In the future it might work out well
// to have their prent be some sort of thread team object
void thread_setup( omp_micro thread_func, void *fp, int tid, parallel_region *team ) {

    omp_thread_data task_data(tid, team);
    auto thread_id = get_self_id();
    set_thread_data( thread_id, reinterpret_cast<size_t>(&task_data));

    thread_func(tid, fp);
    team->num_tasks--;
    if(team->num_tasks == 0) {
        team->cond.notify_all();
    }
    //How do I know if this thread can return without task_exit()?
    //I think the cancel barrier does this
}

void fork_worker( omp_micro thread_func, frame_pointer_t fp, parallel_region &team) {
    vector<hpx::lcos::future<void>> threads;
    for(int i = 0; i < team.nthreads_var; i++) {
        team.num_tasks++;
        threads.push_back( hpx::async( thread_setup, *thread_func, fp, i, &team));
    }
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(team.thread_mtx);
        while(team.num_tasks > 0) {
            team.cond.wait(lock);
        }
    }
    hpx::wait_all(threads);
}

void fork_and_sync( parallel_region &team, omp_micro thread_func,
                        frame_pointer_t fp, boost::mutex& mtx, 
                        boost::condition& cond, bool& running) {

    //parallel_region team(num_threads);
    fork_worker(thread_func, fp, team);

    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}
 
//For Intel, the Nthreads isn't passed in, another function sets Nthreads, so Nthreads should be 0;
// Also for Intel, fp is not a frame pointer, but a pointer to a struct,
// but is still passed around correctly internally.

//TODO: according to the spec, the current thread should be thread 0 of the new team, and execute the new work.

void hpx_runtime::fork(int Nthreads, omp_micro thread_func, frame_pointer_t fp)
{ 
    parallel_region team = get_team()->make_child_region(Nthreads);

    if( hpx::threads::get_self_ptr() ) {
        //parallel_region team(get_team(), get_team()->request_threads(Nthreads));
        fork_worker(thread_func, fp, team);
    } else {

        //implicit_region.reset(new parallel_region(initial_num_threads));
        //if(Nthreads <= 0){
        //    Nthreads = implicit_region->nthreads_var;
        //}
        boost::mutex mtx;
        boost::condition cond;
        bool running = false;
    
        hpx::applier::register_thread_nullary(
                std::bind(&fork_and_sync, team, thread_func, fp, boost::ref(mtx), boost::ref(cond), boost::ref(running))
                //std::bind(&fork_and_sync, boost::ref(team), thread_func, fp, boost::ref(mtx), boost::ref(cond), boost::ref(running))
                , "ompc_fork_worker");
        {   // Wait for the thread to run.
            boost::mutex::scoped_lock lk(mtx);
            while (!running)
                cond.wait(lk);
        }
    }
}

