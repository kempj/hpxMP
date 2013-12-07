#include "hpx_runtime.h"


int hpx_main() {
    assert(false);
    return 1;
}

void wait_for_startup(boost::mutex& mtx, boost::condition& cond, bool& running){
    cout << "HPX OpenMP runtime has started" << endl;
    // Let the main thread know that we're done.
    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}

void fini_runtime_worker(boost::mutex& mtx,
        boost::condition& cond, bool& running) {
    hpx::stop();
}

void fini_runtime() {
    cout << "Stopping HPX OpenMP runtime" << endl;

    boost::mutex mtx;
    boost::condition cond;
    bool running = false;

    hpx::applier::register_thread_nullary(
        HPX_STD_BIND(&fini_runtime_worker, 
            boost::ref(mtx), boost::ref(cond), boost::ref(running))
      , "fini_runtime_worker");
}

void hpx_runtime::init(int Nthreads) {
    mutex_type::scoped_lock l(init_mtx);

    if (hpx_initialized)
        return;
    if(Nthreads > 0)
        num_threads = Nthreads;
    else
        num_threads = hpx::threads::hardware_concurrency();

    globalBarrier = new barrier(num_threads);
    cout << "Starting HPX OpenMP runtime" << endl; 

    using namespace boost::assign;
    std::vector<std::string> cfg;
    cfg += "hpx.os_threads=" + boost::lexical_cast<std::string>(num_threads);
    cfg += "hpx.run_hpx_main!=0";

    char const* hpx_args_raw = getenv("OMP_HPX_ARGS");

    int argc;
    char ** argv;

    if (hpx_args_raw) { 
        std::string tmp(hpx_args_raw);

        std::vector<std::string> hpx_args;
        boost::algorithm::split(hpx_args, tmp,
            boost::algorithm::is_any_of(";"),
                boost::algorithm::token_compress_on);

        // FIXME: For correctness check for signed overflow.
        argc = hpx_args.size() + 2;
        argv = new char*[argc];

        // FIXME: Should we do escaping?    
        for (boost::uint64_t i = 0; i < hpx_args.size(); ++i) {
            cout << "arg[" << i << "]: " << hpx_args[i] << endl;
            argv[i + 1] = strdup(hpx_args[i].c_str());
        }
    } else {
        argc = 2;
        argv = new char*[argc];
    }
    argv[0] = const_cast<char*>("hpxMP");
    argv[argc - 1] = const_cast<char*>("-Ihpx.stacks.use_guard_pages=0");
    HPX_STD_FUNCTION<int(boost::program_options::variables_map& vm)> f;
    boost::program_options::options_description desc_cmdline; 

    boost::mutex local_mtx;
    boost::condition cond;
    bool running = false;

    hpx::start(f, desc_cmdline, argc, argv, cfg,
        HPX_STD_BIND(&wait_for_startup, 
            boost::ref(local_mtx), boost::ref(cond), boost::ref(running)));

    { // Wait for the thread to run.
        boost::mutex::scoped_lock lk(local_mtx);
        if (!running)
            cond.wait(lk);
    }
    atexit(fini_runtime);
    delete[] argv;
    hpx_initialized = true;
}

void thread_setup(void (*micro_task)(int, void*), int thread_num, void *fp) {
    thread_data *data_struct = new thread_data;
    data_struct->thread_num = thread_num;
    auto thread_id = hpx::threads::get_self_id();
    hpx::threads::set_thread_data( thread_id, reinterpret_cast<size_t>(data_struct));
    micro_task(thread_num, fp);
}


void ompc_fork_worker(int Nthreads, omp_micro micro_task, frame_pointer_t fp,
    boost::mutex& mtx, boost::condition& cond, bool& running)
{
    vector<hpx::lcos::future<void>> threads;
    threads.reserve(Nthreads);
    for(int i = 0; i < Nthreads; i++) {
        threads.push_back( hpx::async(thread_setup, *micro_task, i, fp));
    }
    hpx::lcos::wait(threads);
    // Let the main thread know that we're done.
    {
        boost::mutex::scoped_lock lk(mtx);
        running = true;
        cond.notify_all();
    }
}




void hpx_runtime::fork(int Nthreads, omp_micro micro_task, frame_pointer_t fp) { 
    boost::mutex mtx;
    boost::condition cond;
    bool running = false;

    if(Nthreads <= 0) {
        Nthreads = num_threads;
    }
    hpx::applier::register_thread_nullary(
            HPX_STD_BIND(&ompc_fork_worker, Nthreads, micro_task, fp,
                boost::ref(mtx), boost::ref(cond), boost::ref(running))
            , "ompc_fork_worker");
    // Wait for the thread to run.
    {
        boost::mutex::scoped_lock lk(mtx);
        if (!running)
            cond.wait(lk);
    }
}




