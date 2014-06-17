//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 7

#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;

extern boost::shared_ptr<mutex_type> print_mtx;

//atomic<double> task_creation_time{0};

atomic<int> num_tasks{0};

boost::shared_ptr<hpx::lcos::local::condition_variable> thread_cond;

void wait_for_startup(boost::mutex& mtx, boost::condition& cond, bool& running){
    cout << "HPX OpenMP runtime has started" << endl;
    // Let the main thread know that we're done.
    {
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
    if(omp_num_threads != NULL){
        num_threads = atoi(omp_num_threads);
    } else { 
        num_threads = num_procs;
    }

    walltime.reset(new high_resolution_timer);
    globalBarrier.reset(new barrier(num_threads));
        
    std::vector<std::string> cfg;
    int argc;
    char ** argv;
    using namespace boost::assign;
    cfg += "hpx.os_threads=" + boost::lexical_cast<std::string>(num_threads);
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

    cout << "Starting HPX OpenMP runtime" << endl; 

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

double hpx_runtime::get_time() {
    return walltime->now();
}

int hpx_runtime::get_num_threads() {
    return num_threads;
}

int hpx_runtime::get_num_procs() {
    return num_procs;
}

void hpx_runtime::set_num_threads(int nthreads) {
    if(nthreads > 0) {
        num_threads = nthreads;
    }
}

//According to the spec, this should only be called from a "thread", 
// and never from inside an openmp tasks.
void hpx_runtime::barrier_wait(){
    //if I increment threads at creation, and decrement them here, 
    // can I remove the barrier completely?
    while(num_tasks > 0){
        hpx::this_thread::yield();
    }
    globalBarrier->wait();
}

int hpx_runtime::get_thread_num() {
    auto thread_id = hpx::threads::get_self_id();
    auto *data = reinterpret_cast<thread_data*>(
                    hpx::threads::get_thread_data(thread_id) );
    return data->thread_num;
}

void hpx_runtime::task_wait() {
    auto *data = reinterpret_cast<thread_data*>(
                hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    hpx::wait_all(data->task_handles);
    data->task_handles.clear();
}

//This needs to be here to make sure to wait for dependant children finish
// before destroying the stack of the task. If this work is done in task_create
// the stack does not get preserved.
void hpx_runtime::task_exit() {
    thread_data *task_data = reinterpret_cast<thread_data*>(
                hpx::threads::get_thread_data(hpx::threads::get_self_id()));

    //lock on thread specific mutex
    //will need thread specific condition
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(task_data->thread_mutex);
        while(task_data->blocking_children > 0)
            task_data->thread_cond.wait(lock);
    }
//    while(task_data->blocking_children > 0) {
//        hpx::this_thread::yield();
//    }
}

void task_setup( omp_task_func task_func, void *fp, void *firstprivates,
                 thread_data *task_data) {
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(task_data));
    int blocks_parent = task_data->blocks_parent;
    thread_data *parent_task = task_data->parent;

    task_func(firstprivates, fp);
    task_data->is_finished = true;    
    if(blocks_parent) {
        parent_task->blocking_children--;
        parent_task->thread_cond.notify_one();
    }
    delete task_data;
    num_tasks--;
    if(num_tasks == 0) {
        thread_cond->notify_all();
    }
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied, 
                               int blocks_parent) {
    auto *parent_task = reinterpret_cast<thread_data*>(
            hpx::threads::get_thread_data(hpx::threads::get_self_id()));

    thread_data *child_task= new thread_data(parent_task);
    child_task->blocks_parent = blocks_parent;
    num_tasks++;
    if(blocks_parent) {
        parent_task->blocking_children += 1;
        parent_task->has_dependents = true;
    }
    parent_task->task_handles.push_back( 
                    hpx::async( task_setup, taskfunc, frame_pointer, 
                                firstprivates, child_task));
}

//Thread tasks currently have no parent. In the future it might work out well
// to have their parent be some sort of thread team object
void thread_setup( omp_task_func task_func, void *fp, int tid, mutex_type& mtx ) {
    thread_data *task_data = new thread_data(tid);
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(task_data));

    task_func((void*)0, fp);
    
    {
        boost::unique_lock<hpx::lcos::local::spinlock> lock(mtx);
        while(num_tasks > 0)
        {
            thread_cond->wait(lock);
        }
    }
    //while(num_tasks > 0) {
    //    hpx::this_thread::yield();
    //}
    delete task_data;
}

void ompc_fork_worker( int num_threads, omp_task_func task_func,
                       frame_pointer_t fp, boost::mutex& mtx, 
                       boost::condition& cond, bool& running) {
    vector<hpx::lcos::future<void>> threads;
    num_tasks = 0;

    mutex_type thread_mtx;
    thread_cond.reset(new hpx::lcos::local::condition_variable);

    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async( thread_setup, *task_func, fp, i, boost::ref(thread_mtx)));
    }
    hpx::wait_all(threads);
    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}
 
void hpx_runtime::fork(int Nthreads, omp_task_func task_func, frame_pointer_t fp) { 
    if(Nthreads > 0)
        threads_requested = Nthreads;
    else
        threads_requested = num_threads;

    boost::mutex mtx;
    boost::condition cond;
    bool running = false;

    hpx::applier::register_thread_nullary(
            //HPX_STD_BIND(&ompc_fork_worker, threads_requested, task_func, fp,
            HPX_STD_BIND(&ompc_fork_worker, num_threads, task_func, fp,
                boost::ref(mtx), boost::ref(cond), boost::ref(running))
            , "ompc_fork_worker");
    {   // Wait for the thread to run.
        boost::mutex::scoped_lock lk(mtx);
        while (!running)
            cond.wait(lk);
    }
}

