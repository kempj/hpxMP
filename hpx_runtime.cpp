//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 7

#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;

#include <atomic>

std::atomic<int> num_tasks(0);

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

    boost::mutex mtx;
    boost::condition cond;

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
    single_mtx_id = new_mtx();
    crit_mtx_id = new_mtx();
    lock_mtx_id = new_mtx();
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

bool hpx_runtime::trylock(int lock_id){
    assert(lock_id < lock_list.size());
    return lock_list[lock_id]->try_lock();
}

void hpx_runtime::lock(int lock_id) {
    assert(lock_id < lock_list.size());
    lock_list[lock_id]->lock();
}

void hpx_runtime::unlock(int lock_id) {
    lock_list[lock_id]->unlock();
}

int hpx_runtime::new_mtx(){
    lock_list.emplace_back(new mutex_type());
    return lock_list.size() - 1;
}

//According to the spec, this should only be called from a "thread", 
// and never from inside an openmp tasks.
// This could potentially be taken advantage of, but currently is not.
void hpx_runtime::barrier_wait(){
    //auto thread_id = hpx::threads::get_self_id();
    //auto *data = reinterpret_cast<thread_data*>(
    //                hpx::threads::get_thread_data(thread_id) );
    //hpx::wait_all(data->task_handles);
    //hpx::wait_all(data->child_tasks);
    //This doesn't wait on all tasks
    while(num_tasks > 0){
        hpx::this_thread::yield();
    }
    //globalBarrier->wait();
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

void task_setup( omp_task_func task_func, void *firstprivates,
                 void *fp, thread_data *parent_task, int blocks_parent) {
    thread_data *task_data = new thread_data(parent_task);
    task_data->blocks_parent = blocks_parent;
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(task_data));

    if(blocks_parent) {
        {
            hpx::lcos::local::spinlock::scoped_lock lk(parent_task->thread_mutex);
            parent_task->blocking_children += 1;
            parent_task->has_dependents = true;
        }
    }

    task_func(firstprivates, fp);

    {//An atomic would probably be better here
        hpx::lcos::local::spinlock::scoped_lock lk(task_data->thread_mutex);
        task_data->is_finished = true;    
    }
    while(task_data->blocking_children > 0) {
        hpx::this_thread::yield();
    }
    if(blocks_parent) {
        {
            hpx::lcos::local::spinlock::scoped_lock lk(parent_task->thread_mutex);
            parent_task->blocking_children -= 1;
        }
    }
    delete task_data;
    num_tasks--;
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied, 
                               int blocks_parent) {
    auto *parent_task = reinterpret_cast<thread_data*>(
            hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    num_tasks++;
    parent_task->task_handles.push_back( 
                    hpx::async( task_setup, taskfunc, firstprivates, 
                                frame_pointer, parent_task, blocks_parent));
}

void ompc_fork_worker( int num_threads, omp_task_func task_func,
                       frame_pointer_t fp, boost::mutex& mtx, 
                       boost::condition& cond, bool& running) {

    vector<hpx::lcos::future<void>> threads;
    vector<thread_data*> thread_data_vector;
    thread_data *tmp_data;

    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        tmp_data = new thread_data(i);
        threads.push_back( hpx::async( task_setup, *task_func, (void*)0, fp, tmp_data, 0));
        thread_data_vector.push_back(tmp_data);
    }
    hpx::wait_all(threads);
    while(num_threads > 0) {
        hpx::this_thread::yield();
    }

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
            HPX_STD_BIND(&ompc_fork_worker, threads_requested, task_func, fp,
                boost::ref(mtx), boost::ref(cond), boost::ref(running))
            , "ompc_fork_worker");
    // Wait for the thread to run.
    {
        boost::mutex::scoped_lock lk(mtx);
        if (!running)
            cond.wait(lk);
    }
}

