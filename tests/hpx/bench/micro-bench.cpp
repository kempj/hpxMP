#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>
//#include <hpx/include/async.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>

#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/format.hpp>

using hpx::lcos::local::barrier;
using hpx::lcos::shared_future;
using hpx::lcos::future;

using std::vector;
using std::cout;
using std::endl;


void print_time(std::vector<uint64_t> time, std::string name) {
    uint64_t total = 0, min = time[0], max = 0;
    for(int i = 0; i < time.size(); i++) {
        total += time[i];
        if(time[i] > max) {
            max = time[i];
        }
        if(time[i] < min) {
            min = time[i];
        }
    }
    cout << "\ntest " << name << ", average = " << (double)total / (double)time.size()
         << "ns - min = " << min << " - max = " << max << endl;
}

void delay(int delaylength) {
    float a = 0.;
    for(int i = 0; i < delaylength; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
}


void parallel_thread(int inner_reps, int delay_length, barrier *B) {
    vector<future<void>> tasks(inner_reps);
    B->wait();
    for(int i=0; i<inner_reps; i++) {
        tasks[i] = hpx::async(delay, delay_length);
    }
    hpx::wait_all(tasks);
    B->wait();
}

//parallel spawn
boost::uint64_t par_region(int num_threads, int inner_reps, int delay_reps) {
    vector<future<void>> threads(num_threads);
    barrier B(num_threads+1);
    for(int i=0; i<num_threads; i++) {
        threads[i] = hpx::async(parallel_thread, inner_reps, delay_reps, &B);
    }
    B.wait();
    boost::uint64_t start = hpx::util::high_resolution_clock::now();
    B.wait();
    uint64_t total = hpx::util::high_resolution_clock::now() - start;
    hpx::wait_all(threads);
    return total;
}

void barrier_func(int delay_reps, barrier *B) {
    B->wait();
    delay(delay_reps);
    B->wait();
}

//barrier
boost::uint64_t barrier_test(int num_threads, int inner_reps, int delay_reps) {
    vector<future<void>> threads;
    barrier B(num_threads);
    for(int i = 0; i < num_threads - 1; i++) {
        threads.push_back(hpx::async(barrier_func, delay_reps, &B));
    }
    B.wait();
    boost::uint64_t start = hpx::util::high_resolution_clock::now();
    B.wait();
    uint64_t total = hpx::util::high_resolution_clock::now() - start;
    hpx::wait_all(threads);
    return total;
}

int hpx_main(boost::program_options::variables_map& vm) {
    std::string test = vm["test"].as<std::string>();
    int reps = vm["reps"].as<int>();
    int num_threads = hpx::get_os_thread_count();
    vector<uint64_t> time(reps);
    int delay_reps = vm["delay_reps"].as<int>();
    int inner_reps = vm["inner_reps"].as<int>();

    cout << "outer_reps = " << reps << endl;
    cout << "inner_reps = " << inner_reps << endl;
    cout << "delay_reps = " << delay_reps << endl;
    cout << "num_threads = " << num_threads << endl;

    if(test == "all" or test == "0") {
        for(int i = 0; i < reps; i++) {
            time[i] = (par_region(num_threads, inner_reps, delay_reps))/(double)inner_reps;
        }
        print_time(time, "parallel region");
    }
    if(test == "all" or test == "1") {
        for(int i = 0; i < reps; i++) {
            time[i] = (barrier_test(num_threads, inner_reps, delay_reps));
        }
        print_time(time, "Barrier");
    }
    return hpx::finalize();
}

int main(int argc, char ** argv) {

    using namespace boost::assign;
    std::vector<std::string> cfg;
    cfg += "hpx.os_threads=" +        
        boost::lexical_cast<std::string>(hpx::threads::hardware_concurrency());

    boost::program_options::options_description
       desc_commandline("Usage: " HPX_APPLICATION_STRING " [options]");

    using boost::program_options::value;
    desc_commandline.add_options()
        ( "reps", value<int>()->default_value(20),
          "number of times to repeat the benchmark")
        ( "inner_reps", value<int>()->default_value(1024),
          "number of tasks each thread creates")
        ( "delay_reps", value<int>()->default_value(10000),
          "number of iterations in the delay function")
        ( "test", value<std::string>()->default_value("all"),
          "select tests to execute (0-?, default: all)") ;


    return hpx::init(desc_commandline, argc, argv, cfg);
}
