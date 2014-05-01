//  Copyright (c) 2013 Jeremy Kemp
//  Copyright (c) 2013 Bryce Adelstein-Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#define  HPX_LIMIT 6

#include "hpx_runtime.h"

extern boost::shared_ptr<hpx_runtime> hpx_backend;

void hpx_runtime::env_init(){
    char const* omp_num_threads = getenv("OMP_NUM_THREADS");
    if(omp_num_threads != NULL){
        num_threads = atoi(omp_num_threads);
    } else { 
        num_threads = 0;
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

hpx_runtime::hpx_runtime() {
    env_init();
    num_procs = hpx::threads::hardware_concurrency();
    if(0 == num_threads) {
        num_threads = num_procs;
    }
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
    int map_size = lock_list.size();
    mtx_ptr temp_mtx(new mutex_type);
    lock_list.push_back(temp_mtx);
    return map_size;
}

void hpx_runtime::barrier_wait(){
    globalBarrier->wait();
}

int hpx_runtime::get_thread_num() {
    auto thread_id = hpx::threads::get_self_id();
    auto *data = reinterpret_cast<thread_data*>(
                    hpx::threads::get_thread_data(thread_id) );
    return data->thread_num;
}

void task_setup( omp_task_func task_func, int thread_num, void *firstprivates,
                 void *fp) {
    thread_data *data_struct = new thread_data();
    data_struct->thread_num = thread_num;
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(data_struct));
    task_func(firstprivates, fp);
}

void hpx_runtime::create_task( omp_task_func taskfunc, void *frame_pointer,
                               void *firstprivates, int is_tied) {
    auto *data = reinterpret_cast<thread_data*>(
            hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    int current_tid = data->thread_num;
    data->task_handles.push_back( hpx::async(task_setup, taskfunc, current_tid, firstprivates, frame_pointer));
}

void hpx_runtime::task_wait() {
    auto *data = reinterpret_cast<thread_data*>(
                hpx::threads::get_thread_data(hpx::threads::get_self_id()));
    hpx::wait_all(data->task_handles);
    data->task_handles.clear();
}

int hpx_main() {
    int num_threads = hpx_backend->threads_requested;
    omp_task_func task_func = hpx_backend->task_func;
    frame_pointer_t fp = hpx_backend->fp;

    vector<hpx::lcos::future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back( hpx::async(task_setup, *task_func, i, (void*)0, fp));
    }
    hpx::wait_all(threads);
    //TODO:Need to delete hpx data structures, like the barrier, locks, 
    // and the vectors of futures
    return hpx::finalize();
}
 
void hpx_runtime::fork(int Nthreads, omp_task_func func, frame_pointer_t parent_fp) { 
    if(Nthreads > 0)
        threads_requested = Nthreads;
    else
        threads_requested = num_threads;

    task_func = func;
    fp = parent_fp;

    //These need to be reset each fork, but if the runtime shuts down,
    // they need to be destroyed before hpx shuts down.
    walltime.reset(new high_resolution_timer);
    globalBarrier.reset(new barrier(num_threads));

    hpx::init(argc, argv, cfg);
}

