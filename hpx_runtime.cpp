//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 7

#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;

atomic<int> barrier_counter{0};
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
        
    std::vector<std::string> cfg;
    int argc;
    char ** argv;
    using namespace boost::assign;
    cfg += "hpx.os_threads=" + boost::lexical_cast<std::string>(initial_num_threads);
    cfg += "hpx.stacks.use_guard_pages=0";
    cfg += "hpx.run_hpx_main!=0";
    cfg += "hpx.stacks.huge_size=0x2000000";

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


    //HPX_STD_FUNCTION<int(boost::program_options::variables_map& vm)> f;
    hpx::util::function_nonser<int(boost::program_options::variables_map& vm)> f;
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
    //if( hpx::threads::get_self_ptr() ){
    if(nthreads > 0) {
        get_task_data()->icv.nthreads = nthreads;
    }
    //} else {//not an hpx thread
       //initial_num_threads = nthreads; 
    //   initial_thread->icv.nthreads = nthreads; 
    //}
}

//According to the spec, this should only be called from a "thread", 
// and never from inside an openmp tasks.
void hpx_runtime::barrier_wait(){
    while(get_team()->num_tasks > get_team()->num_threads){
        hpx::this_thread::yield();
    }
    barrier_counter++;
    cout << "barrier counter = " << barrier_counter << endl;
    get_team()->globalBarrier.wait();
    cout << "after barrier " << endl;

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

//This needs to be here to make sure to wait for dependant children finish
// before destroying the stack of the task. If this work is done in task_create
// the stack of the user's task does not get preserved.
// Note: in OpenUH this gets called at the end of implicit and explicit tasks
void hpx_runtime::task_exit() {
    auto *task_data = get_task_data();
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(task_data->thread_mutex);
        while(task_data->blocking_children > 0) {
            task_data->thread_cond.wait(lock);
        }
    }
}

//Maybe have team hand out task id's round robin?
void intel_task_setup( kmp_routine_entry_t task_func, int gtid, void *task,
                       omp_task_data *task_data, parallel_region *team, int thread_num) {

    set_thread_data( get_self_id(), reinterpret_cast<size_t>(task_data));

    task_func(gtid, task);

    team->num_tasks--;
    if(team->num_tasks == 0) {
        team->cond.notify_all();
    }
    //FIXME: these explicit tasks do not wait for child tasks
    //kmp_task_t *task = (kmp_task_t*)new char[sizeof_kmp_task_t + sizeof_shareds];
    delete[] (char*)task;//it was allocated as char
    delete task_data;
    cout << "implicit task " << gtid << " going out of scope" << endl;
}

void hpx_runtime::create_intel_task( kmp_routine_entry_t task_func, int gtid, void *task){
    auto *parent_task = get_task_data();
    omp_task_data *child_task = new omp_task_data(parent_task);
    parent_task->team->num_tasks++;
    parent_task->task_handles.push_back( 
                    hpx::async( intel_task_setup, task_func, gtid, task, child_task,
                                parent_task->team, parent_task->thread_num));
}

void task_setup( omp_task_func task_func, void *fp, void *firstprivates,
                 omp_task_data *task_data, omp_task_data *parent_task, int blocks_parent) {
    auto thread_id = get_self_id();
    //omp_task_data task_data(thread_num, team, parent);
    set_thread_data( thread_id, reinterpret_cast<size_t>(&task_data));

    task_func(firstprivates, fp);

    task_data->is_finished = true;    
    if(blocks_parent) {
        parent_task->blocking_children--;
        if(parent_task->blocking_children == 0) {
            parent_task->thread_cond.notify_one();
        }
    }
    task_data->team->num_tasks--;
    if(task_data->team->num_tasks == 0) {
        task_data->team->cond.notify_all();
    }
}

//Tasks get bound to their parent's team, unlike implicit threads
void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied, 
                               int blocks_parent) {
    auto *parent = get_task_data();
    omp_task_data *child_task = new omp_task_data(parent);//TODO: when is this de-allocated?
    parent->team->num_tasks++;
    if(blocks_parent) {
        parent->blocking_children += 1;
        parent->has_dependents = true;
    }
    parent->task_handles.push_back( 
                    hpx::async( task_setup, taskfunc, frame_pointer, 
                                firstprivates, child_task, parent, blocks_parent));
}

void thread_setup( omp_micro thread_func, void *fp, int tid,
                    parallel_region *team, omp_task_data *parent ) {

    omp_task_data task_data(tid, team, parent);
    auto thread_id = get_self_id();
    set_thread_data( thread_id, reinterpret_cast<size_t>(&task_data));

    thread_func(tid, fp);
    team->num_tasks--;
    if(team->num_tasks == 0) {
        team->cond.notify_all();
    }
}

void fork_worker( omp_micro thread_func, frame_pointer_t fp,
                    parallel_region *team, omp_task_data *parent) {
    vector<hpx::lcos::future<void>> threads;
    for(int i = 0; i < parent->threads_requested; i++) {
        team->num_tasks++;
        threads.push_back( hpx::async( thread_setup, *thread_func, fp, i, team, parent));
    }
    //TODO: does this need to go after the wait_all?
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(team->thread_mtx);
        while(team->num_tasks > 0) {
            team->cond.wait(lock);
        }
    }
    hpx::wait_all(threads);
}

void fork_and_sync( parallel_region *team, omp_micro thread_func,
                        frame_pointer_t fp, omp_task_data *parent,
                        boost::mutex& mtx, boost::condition& cond, 
                        bool& running) {

    fork_worker(thread_func, fp, team, parent);
    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}
 
//For Intel, the Nthreads isn't passed in, another function sets Nthreads, so Nthreads should be 0;
// Also for Intel, fp is not a frame pointer, but a pointer to a struct,
//TODO: according to the spec, the current thread should be thread 0 of the new team, and execute the new work.
void hpx_runtime::fork(int Nthreads, omp_micro thread_func, frame_pointer_t fp)
{ 
    omp_task_data *current_task = get_task_data();
    current_task->set_threads_requested( Nthreads );

    //does this always need to be allocated in an hpx thread?
    parallel_region team(current_task->team, current_task->threads_requested);

    if( hpx::threads::get_self_ptr() ) {
        fork_worker(thread_func, fp, &team, current_task);
    } else {

        boost::mutex mtx;
        boost::condition cond;
        bool running = false;
    
        hpx::applier::register_thread_nullary(
                std::bind(&fork_and_sync, &team, thread_func, fp, current_task, boost::ref(mtx), boost::ref(cond), boost::ref(running))
                , "ompc_fork_worker");
        {   // Wait for the thread to run.
            boost::mutex::scoped_lock lk(mtx);
            while (!running)
                cond.wait(lk);
        }
    }
    cout << "parallel region going out of scope" << endl;
}

