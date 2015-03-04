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

const int DEPTH = 6;
int delay_length;

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
    cout << endl << name << ", (average, min, max) in ns:" << endl
         << (total / time.size()) << ", " << min << ", " << max << endl;
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
    uint64_t starttime, lapsedtime = 0;
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
future<void> leafTaskTree(int tree_level);
void leaf2(int tree_level) {
    leafTaskTree(tree_level - 1);
    leafTaskTree(tree_level - 1);
}

future<void> leafTaskTree(int tree_level) {
    if( tree_level == 0 ) {
        delay(delay_length);
        return hpx::make_ready_future();
    } else {
        return hpx::async(leaf2, tree_level);
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

int hpx_main(boost::program_options::variables_map& vm) {
    std::string test = vm["test"].as<std::string>();
    int reps = vm["reps"].as<int>();
    int inner_reps = vm["inner-reps"].as<int>();
    int num_threads = hpx::get_os_thread_count();
    vector<uint64_t> time(reps);
    delay_length = getdelaylengthfromtime(100);//.1 microseconds


    //if(test == "all" or test == "0") {
    for(int i = 0; i < reps; i++) {
        time[i] = testParallelTaskGeneration(num_threads, inner_reps);
    }
    print_time(time, "PARALLEL TASK");
    //}
    for(int i = 0; i < reps; i++) {
        time[i] = testMasterTaskGeneration(num_threads, inner_reps);
    }
    print_time(time, "MASTER TASK");

    for(int i = 0; i < reps; i++) {
        time[i] = testMasterTaskGenerationWithBusySlaves(num_threads, inner_reps);
    }
    print_time(time, "MASTER TASK BUSY SLAVES");

    for(int i = 0; i < reps; i++) {
        time[i] = testTaskWait(num_threads, inner_reps);
    }
    print_time(time, "TASK WAIT");

    for(int i = 0; i < reps; i++) {
        time[i] = testNestedTaskGeneration(num_threads, inner_reps);
    }
    print_time(time, "NESTED TASK");

    for(int i = 0; i < reps; i++) {
        time[i] = testNestedMasterTaskGeneration(num_threads, inner_reps);
    }
    print_time(time, "NESTED MASTER TASK");

    for(int i = 0; i < reps; i++) {
        time[i] = testBranchTaskGeneration(num_threads, inner_reps);
    }
    print_time(time, "BRANCH TASK TREE");

    for(int i = 0; i < reps; i++) {
        time[i] = testLeafTaskGeneration(num_threads, inner_reps);
    }
    print_time(time, "LEAF TASK TREE");


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
          "select tests to execute (0-?, default: all)") 
        ( "inner-reps", value<int>()->default_value(256),
          "number of times to iterate through loops in the benchmark") ;

    return hpx::init(desc_commandline, argc, argv, cfg);
}
