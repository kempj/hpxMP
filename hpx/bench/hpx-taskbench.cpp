#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>
//#include <hpx/include/async.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>
#include <hpx/util/unwrapped.hpp>

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
    cout << "\ntest " << name << ", average = " << total / time.size()
         << "ns, min = " << min << ", max = " << max << endl;
}

void delay(int delaylength) {
    float a = 0.;
    for(int i = 0; i < delaylength; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
}

int getdelaylengthfromtime(uint64_t delaytime) {
    int delaylength = 0;
    int reps = 1000;
    boost::uint64_t starttime, lapsedtime = 0;
    delaytime = delaytime/1.0E6; // convert from microseconds to seconds
    delay(delaylength);

    while(lapsedtime < delaytime) {
        delaylength = delaylength * 1.1 + 1;
        starttime = hpx::util::high_resolution_clock::now();
        for(int i = 0; i < reps; i++) {
            delay(delaylength);
        }
        lapsedtime = (hpx::util::high_resolution_clock::now() - starttime) / (double) reps;
    }
    return delaylength;
}

//parallel spawn
boost::uint64_t par_region(int num_threads, int delay_length) {
    boost::uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(delay, delay_length));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

void barrier_func(int delay_length, barrier *B) {
    B->wait();
    delay(delay_length);
    B->wait();
}

//PARALLEL TASK
void testParallelTaskGeneration() {
    //spawn N threads (as in a parallel region)
    //from each thread, spawn innerreps tasks
}

//MASTER TASK
void testMasterTaskGeneration() {
    //spawn n * innerreps tasks from one thread
}

//MASTER TASK BUSY SLAVES
void testMasterTaskGenerationWithBusySlaves() {
    //spawn n threads, where 0 spawns innerreps tasks
    // and n-1 threads delay innerreps times
}

future<void> tw_func(int num_iter, int delay_length) {
    vector<future<void>> tasks;
    for(int i = 0; i < num_iter; i++) {
        tasks.push_back(hpx::async(delay, delay_length));
    }
    return hpx::when_all(tasks);
}

//TASK WAIT
void testTaskWait(){
    //each thread does a for over innerreps/nthreads
    //  spawning  a task that spawns nthreads tasks
    //  then wait on all tasks.
}

//NESTED TASK
void testNestedTaskGeneration() {
}

//NESTED MASTER TASK
void testNestedMasterTaskGeneration() {
}

//BRANCH TASK TREE 
void testBranchTaskGeneration() {
}

//LEAF TASK TREE
void testLeafTaskGeneration() {
}

int hpx_main(boost::program_options::variables_map& vm) {
    std::string test = vm["test"].as<std::string>();
    int reps = vm["reps"].as<int>();
    int num_threads = hpx::get_os_thread_count();
    vector<uint64_t> time(reps);
    int default_delay = getdelaylengthfromtime(100);//.1 microseconds


    if(test == "all" or test == "0") {
        for(int i = 0; i < reps; i++) {
            time[i] = par_region(num_threads, default_delay);
        }
        print_time(time, "parallel region");
    }
    if(test == "all" or test == "1") {
        for(int i = 0; i < reps; i++) {
            //time[i] =;
        }
        print_time(time, "");
    }


    return hpx::finalize(); // Handles HPX shutdown
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
        ( "test", value<std::string>()->default_value("all"),
          "select tests to execute (0-?, default: all)") ;


    return hpx::init(desc_commandline, argc, argv, cfg);
}
