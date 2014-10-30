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
    //cout << "HPX OpenMP runtime has started" << endl;
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
    num_procs = hpx::threads::hardware_concurrency();
    char const* omp_num_threads = getenv("OMP_NUM_THREADS");

    //This will break if the users shuts down hpx and an OpenMP call is made
    //TODO: move this code to an init function, and add checks at each entrance
    // to the hpxMP runtime.
    external_hpx = !(hpx::get_runtime_ptr() == NULL);
    if(external_hpx) {
        num_procs = hpx::get_os_thread_count();
    }

    if(omp_num_threads != NULL){
        nthreads_var = atoi(omp_num_threads);
    } else { 
        nthreads_var = num_procs;
    }

    walltime.reset(new high_resolution_timer);

    if(external_hpx)
        return;
        
    std::vector<std::string> cfg;
    int argc;
    char ** argv;
    using namespace boost::assign;
    cfg += "hpx.os_threads=" + boost::lexical_cast<std::string>(nthreads_var);
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

    //cout << "Starting HPX OpenMP runtime" << endl; 

    hpx::start(f, desc_cmdline, argc, argv, cfg,
            HPX_STD_BIND(&wait_for_startup, 
                boost::ref(local_mtx), boost::ref(cond), boost::ref(running)));

    { // Wait for the thread to run.
        boost::mutex::scoped_lock lk(local_mtx);
        if (!running)
            cond.wait(lk);
    }
    //Must be called while hpx is running
    atexit(fini_runtime);

    delete[] argv;
}

parallel_region* hpx_runtime::get_team(){
    auto *task_data = reinterpret_cast<omp_data*>(get_thread_data(get_self_id()));
    return task_data->team;
}

double hpx_runtime::get_time() {
    return walltime->now();
}

int hpx_runtime::get_num_threads() {
    return nthreads_var;
}

int hpx_runtime::get_num_procs() {
    return num_procs;
}

void hpx_runtime::set_num_threads(int nthreads) {
    if(nthreads > 0) {
        nthreads_var = nthreads;
    }
}

//According to the spec, this should only be called from a "thread", 
// and never from inside an openmp tasks.
void hpx_runtime::barrier_wait(){
    auto *task_data = reinterpret_cast<omp_data*>(get_thread_data(get_self_id()));
    while(task_data->team->num_tasks > 0){
        hpx::this_thread::yield();
    }
    task_data->team->globalBarrier.wait();
}

int hpx_runtime::get_thread_num() {
    auto thread_id = get_self_id();
    auto *data = reinterpret_cast<omp_data*>( get_thread_data(thread_id) );
    return data->thread_num;
}

void hpx_runtime::task_wait() {
    auto *data = reinterpret_cast<omp_data*>(get_thread_data(get_self_id()));
    hpx::wait_all(data->task_handles);
    data->task_handles.clear();
}

//This needs to be here to make sure to wait for dependant children finish
// before destroying the stack of the task. If this work is done in task_create
// the stack of the user's task does not get preserved.
void hpx_runtime::task_exit() {
    auto *task_data = reinterpret_cast<omp_data*>(get_thread_data(get_self_id()));
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(task_data->thread_mutex);
        while(task_data->blocking_children > 0) {
            task_data->thread_cond.wait(lock);
        }
    }
}

void task_setup( omp_task_func task_func, void *fp, void *firstprivates,
                 omp_data *parent_task, int blocks_parent) {
    auto thread_id = get_self_id();
    omp_data task_data(parent_task);
    //Can this go out of scope when the function ends, with child tasks still depending on it?
    //It looks fine. Nothing aside from blocking children access their parent at all
    //I don't think this is correct. The thread id gets accessed above, if the parent has been
    // deallocated, this could segfault or return bad info. FIXME
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
        task_data.team->thread_cond.notify_all();
    }
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied, 
                               int blocks_parent) {
    auto *parent_task = reinterpret_cast<omp_data*>(get_thread_data(get_self_id()));
    parent_task->team->num_tasks++;
    if(blocks_parent) {
        parent_task->blocking_children += 1;
        parent_task->has_dependents = true;
    }
    parent_task->task_handles.push_back( 
                    hpx::async( task_setup, taskfunc, frame_pointer, 
                                firstprivates, parent_task, blocks_parent));
}

//Thread tasks currently have no parent. In the future it might work out well
// to have their parent be some sort of thread team object
//void thread_setup( omp_micro thread_func, void *fp, int tid, mutex_type& mtx ) {
void thread_setup( omp_micro thread_func, void *fp, int tid, parallel_region *team ) {
    omp_data task_data(tid, team);
    auto thread_id = get_self_id();
    set_thread_data( thread_id, reinterpret_cast<size_t>(&task_data));

    thread_func(tid, fp);
    
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(task_data.team->thread_mtx);
        while(task_data.team->num_tasks > 0) {
            task_data.team->thread_cond.wait(lock);
        }
    }
}

void fork_worker( int num_threads, 
                  omp_micro thread_func, 
                  frame_pointer_t fp) {
    vector<hpx::lcos::future<void>> threads;
    parallel_region team(num_threads);

    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async( thread_setup, *thread_func, fp, i, &team));
    }
    hpx::wait_all(threads);
}

void fork_and_sync( int num_threads, omp_micro thread_func,
                        frame_pointer_t fp, boost::mutex& mtx, 
                        boost::condition& cond, bool& running) {

    fork_worker( num_threads, thread_func, fp);

    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}
 
//TODO: there is no reason to convert the microtask to a task_func any more.
// It just gets passed to a thread init function.
void hpx_runtime::fork(int Nthreads, omp_micro thread_func, frame_pointer_t fp) { 
    if(Nthreads > 0)
        threads_requested = Nthreads;
    else
        threads_requested = nthreads_var;

    if( hpx::threads::get_self_ptr() ){//Should this be replaced with a get_thread_ptr?
        fork_worker(threads_requested, thread_func, fp);
    } else {
        boost::mutex mtx;
        boost::condition cond;
        bool running = false;
    
        hpx::applier::register_thread_nullary(
                HPX_STD_BIND(&fork_and_sync, threads_requested, thread_func, fp,
                    boost::ref(mtx), boost::ref(cond), boost::ref(running))
                , "ompc_fork_worker");
        {   // Wait for the thread to run.
            boost::mutex::scoped_lock lk(mtx);
            while (!running)
                cond.wait(lk);
        }
    }
}

