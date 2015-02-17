#include <hpx/hpx_init.hpp>
//#include <hpx/include/lcos.hpp>
//#include <hpx/include/async.hpp>
//#include <hpx/lcos/local/barrier.hpp>
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


void delay(int delaylength) {
    float a = 0.;
    for(int i = 0; i < delaylength; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
}

int getdelaylengthfromtime(double delaytime) {
    int delaylength;
    int i, reps;
    double lapsedtime, starttime; // seconds
    reps = 1000;
    lapsedtime = 0.0;
    delaytime = delaytime/1.0E6; // convert from microseconds to seconds
    delaylength = 0;
    delay(delaylength);

    while (lapsedtime < delaytime) {
        delaylength = delaylength * 1.1 + 1;
        starttime = getclock();
        for (i = 0; i < reps; i++) {
            delay(delaylength);
        }
        lapsedtime = (getclock() - starttime) / (double) reps;
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

//barrier
//single
//master
//lock/unlock
//par for
//for static
//for dynamic
//simple task creation
//task wait
//more complex task creation

int hpx_main(boost::program_options::variables_map& vm) {
    std::string test = vm["test"].as<std::string>();
    int reps = vm["reps"].as<int>();
    int num_threads = hpx::get_os_thread_count();
    vector<uint64_t> time(reps);
    int sched_delay   = getdelaylengthfromtime(15.0);//in microseconds. from EPCC
    int default_delay = getdelaylengthfromtime(.1);


    if(test == "all" or test == "0") {
        for(int i = 0; i < reps; i++) {
            par_region(
        }
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
