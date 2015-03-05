#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>
//#include <hpx/include/async.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>
#include <hpx/util/unwrapped.hpp>

#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/format.hpp>

using hpx::lcos::shared_future;
using hpx::lcos::future;

using std::vector;
using std::cout;
using std::endl;

const int DEPTH = 6;
int delay_length;


void delay(int delaylength) {
    float a = 0.;
    for(int i = 0; i < delaylength; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
}

//PARALLEL TASK
future<void> spawn_tasks(int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        tasks.push_back(hpx::async(delay, delay_length));
    }
    return hpx::when_all(tasks);
}
uint64_t testParallelTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(spawn_tasks(inner_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//MASTER TASK
uint64_t testMasterTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads * inner_reps);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(delay, delay_length));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//MASTER TASK BUSY SLAVES
future<void> master_busy_thread(int thread_id, int inner_reps) {
    vector<future<void>> tasks;
    if(thread_id == 0) {
        for(int i = 0; i < inner_reps; i++) {
            tasks.push_back(hpx::async(delay, delay_length));
        }
    } else {
        delay(inner_reps);
    }
    return hpx::when_all(tasks);
}
uint64_t testMasterTaskGenerationWithBusySlaves(int num_threads, int inner_reps)  {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(master_busy_thread(i, inner_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//TASK WAIT
void spawn_tasks_wait(int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        tasks.push_back(hpx::async(delay, delay_length));
    }
    hpx::wait_all(tasks);
}
// this is very similar to PARALLEL TASK, only with taskwaits in each thread
// this is probably going to be slower, though.
uint64_t testTaskWait(int num_threads, int inner_reps){
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(spawn_tasks_wait, inner_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//NESTED TASK
void spawn_nested_tasks_wait(int num_threads, int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        tasks.push_back(spawn_tasks( num_threads ));
    }
    hpx::wait_all(tasks);
}
uint64_t testNestedTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(spawn_nested_tasks_wait, num_threads, inner_reps/num_threads));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//NESTED MASTER TASK
uint64_t testNestedMasterTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        threads.push_back(spawn_tasks( num_threads ));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//BRANCH TASK TREE 
future<void> branch2(int tree_level);

future<void> branch1(int tree_level) {
    future<void> f = branch2(tree_level - 1);
    delay(delay_length);
    return f;
}
future<void> branch2(int tree_level) {
    vector<future<void>> sub;
    if(tree_level > 0) {
        sub.push_back(branch2(tree_level - 1));
        sub.push_back(branch2(tree_level - 1));
        delay(delay_length);
    }
    return hpx::when_all(sub);
}
future<void> branch_thread_func(int inner_reps) {
    vector<future<void>> tasks;
    for(int i = 0; i < (inner_reps >> DEPTH); i++) {
        tasks.push_back(hpx::async(branch1, DEPTH));
    }
    return hpx::when_all(tasks);
}
uint64_t testBranchTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(branch_thread_func( inner_reps ));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//LEAF TASK TREE
future<void> leaf_task_tree(int tree_level) {
    if( tree_level == 0 ) {
        delay(delay_length);
        return hpx::make_ready_future();
    } else {
        auto f1 = hpx::async(leaf_task_tree, tree_level-1);
        auto f2 = hpx::async(leaf_task_tree, tree_level-1);
        return hpx::when_all(f1, f2);
    } 
}
future<void> leaf_thread_func(int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps >> DEPTH);
    for(int i = 0; i < (inner_reps >> DEPTH); i++) {
        tasks.push_back(leaf_task_tree(DEPTH));
    }
    return hpx::when_all(tasks);
}
uint64_t testLeafTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(leaf_thread_func( inner_reps ));
    }
    return hpx::util::high_resolution_clock::now() - start;
}

void print_time(std::vector<double> time, std::string name) {
    double total = 0, min = time[0], max = 0;
    for(int i = 0; i < time.size(); i++) {
        total += time[i];
        if(time[i] > max) {
            max = time[i];
        }
        if(time[i] < min) {
            min = time[i];
        }
    }
    cout << endl << name << ", (average, min, max) in ns:" << endl
         << (total / time.size()) << ", " << min << ", " << max << endl;
}

int hpx_main(boost::program_options::variables_map& vm) {
    std::string test = vm["test"].as<std::string>();
    int reps = vm["reps"].as<int>();
    int timing_version = vm["timings"].as<int>();
    int delay_time = vm["delay_time"].as<int>();
    int inner_reps = 20;
    int num_threads = hpx::get_os_thread_count();
    vector<double> time(reps);
    //delay_length = getdelaylengthfromtime(delay_time);
    delay_length = 490;

    if(timing_version > 1) {
        inner_reps *=  timing_version;
    }
    
    for(int i = 0; i < reps; i++) {
        time[i] = testParallelTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "PARALLEL TASK");//20

    for(int i = 0; i < reps; i++) {
        time[i] = testMasterTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "MASTER TASK");//20

    if(timing_version == 1) {
        inner_reps = 640;
    } else {
        inner_reps = 1280;
    }
    if(timing_version > 1) {
        inner_reps *= timing_version;
    }
    for(int i = 0; i < reps; i++) {
        time[i] = testMasterTaskGenerationWithBusySlaves(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "MASTER TASK BUSY SLAVES");//1280 / 640 (hpxMP)

    if(timing_version == 1) {
        inner_reps = 80;
    } else {
        inner_reps = 1280;
    }
    if(timing_version > 1) {
        inner_reps *= timing_version;
    }
    for(int i = 0; i < reps; i++) {
        time[i] = testTaskWait(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "TASK WAIT");//1280 / 80 (hpxMP)

    for(int i = 0; i < reps; i++) {
        time[i] = testNestedTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "NESTED TASK");//2560 / 80 (hpxMP)

    if(timing_version == 1) {
        inner_reps = 160;
    } else {
        inner_reps = 1280;
    }
    if(timing_version > 1) {
        inner_reps *= timing_version;
    }
    for(int i = 0; i < reps; i++) {
        time[i] = testNestedMasterTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "NESTED MASTER TASK");//2560 / 160 (hpxMP)

    for(int i = 0; i < reps; i++) {
        time[i] = testBranchTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "BRANCH TASK TREE");//5120 / 160 (hpxMP)

    for(int i = 0; i < reps; i++) {
        time[i] = testLeafTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "LEAF TASK TREE");//5120 / 160 (hpxMP)


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
        ( "delay_time", value<int>()->default_value(100),
          "amount of time to do work in a task, in ns (default 100ns)")
        ( "delay_length", value<std::string>()->default_value("490"),
          "number of iterations in delay function (0-?, default: 490)") 
        ( "test", value<std::string>()->default_value("all"),
          "select tests to execute (0-?, default: all)") 
        ( "timings", value<int>()->default_value(0),
          "0 to mimic inner reps for icc, 1 for hpxMP") ;

    return hpx::init(desc_commandline, argc, argv, cfg);
}

