//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 7

#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;

hpx_runtime::hpx_runtime() {
    num_procs = hpx::threads::hardware_concurrency();
    char const* omp_num_threads = getenv("OMP_NUM_THREADS");
    if(omp_num_threads != NULL){
        num_threads = atoi(omp_num_threads);
    } else { 
        num_threads = num_procs;
    }
        
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
}

double hpx_runtime::get_time() {
    return walltime->now();
}

int hpx_runtime::get_num_threads() {
    return num_threads;
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

void hpx_runtime::barrier_wait(){
    //hpx::wait_all(data->child_tasks)
    globalBarrier->wait();
}

int hpx_runtime::get_thread_num() {
    auto thread_id = hpx::threads::get_self_id();
    auto *data = reinterpret_cast<thread_data*>(
                    hpx::threads::get_thread_data(thread_id) );
    return data->thread_num;
}

void mtx_setup() {
    hpx_backend->single_mtx_id = hpx_backend->new_mtx();
    hpx_backend->crit_mtx_id = hpx_backend->new_mtx();
    hpx_backend->lock_mtx_id = hpx_backend->new_mtx();
}

void hpx_runtime::task_wait() {
    auto *data = reinterpret_cast<thread_data*>(
                hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    hpx::wait_all(data->task_handles);
    data->task_handles.clear();
}

void wait_on_tasks(thread_data *data_struct) {
    hpx::wait_all(data_struct->task_handles);
    //At this point, all child tasks must be finished, 
    // so nothing will be appended to child tasks
    hpx::wait_all(data_struct->child_tasks);
}

void task_setup( omp_task_func task_func, void *firstprivates,
                 void *fp, thread_data *parent_data) {
    //TODO: use shared_ptr, since these are never de-allocated
    thread_data *data_struct = new thread_data(parent_data->thread_num);
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(data_struct));
    task_func(firstprivates, fp);
    shared_future<void> child_task = hpx::async(wait_on_tasks, data_struct);
    {
        hpx::lcos::local::spinlock::scoped_lock lk(parent_data->thread_mutex);
        //shared_future<vector<shared_future<void>>> wait_future = hpx::when_all(data_struct->task_handles);
        parent_data->child_tasks.push_back(child_task);
    }
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied) {
    auto *data = reinterpret_cast<thread_data*>(
            hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    //int current_tid = data->thread_num;
    data->task_handles.push_back( hpx::async(task_setup, taskfunc, firstprivates, frame_pointer, data));
}

int hpx_main() {
    vector<hpx::lcos::future<void>> threads;
    int num_threads = hpx_backend->threads_requested;

    mtx_setup();

    hpx_backend->walltime.reset(new high_resolution_timer);
    hpx_backend->globalBarrier.reset(new barrier(num_threads));

    omp_task_func task_func = hpx_backend->task_func;
    frame_pointer_t fp = hpx_backend->fp;

    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async( task_setup, *task_func, (void*)0, fp, new thread_data(i)));
    }
    hpx::wait_all(threads);
    hpx_backend->walltime.reset();
    hpx_backend->globalBarrier.reset();
    hpx_backend->lock_list.clear();
    return hpx::finalize();
}
 
void hpx_runtime::fork(int Nthreads, omp_task_func func, frame_pointer_t parent_fp) { 
    if(Nthreads > 0)
        threads_requested = Nthreads;
    else
        threads_requested = num_threads;

    task_func = func;
    fp = parent_fp;

    hpx::init(argc, argv, cfg);
}

