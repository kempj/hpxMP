#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>
//#include <hpx/include/async.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>
#include <hpx/util/unwrapped.hpp>

#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/format.hpp>

#include <atomic>

using hpx::lcos::shared_future;
using hpx::lcos::future;

using std::vector;
using std::cout;
using std::endl;
using std::atomic;

const int DEPTH = 6;
int delay_reps = 10000;

atomic<int> task_counter{0};

void delay(int delaylength) {
    float a = 0.;
    for(int i = 0; i < delaylength; i++)
        a += i;
    if(a < 0)
        printf("%f \n", a);
    //task_counter++;
}

void spawn_tasks_wait(int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        tasks.push_back(hpx::async(delay, delay_reps));
    }
    hpx::wait_all(tasks);
}

//PARALLEL TASK
future<void> spawn_tasks(int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        tasks.push_back(hpx::async(delay, delay_reps));
    }
    return hpx::when_all(tasks);
}

uint64_t testParallelTaskGeneration(int num_threads, int inner_reps) {
    vector<future<void>> threads;
    threads.reserve(num_threads);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(spawn_tasks, inner_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//MASTER TASK
uint64_t testMasterTaskGeneration(int num_threads, int inner_reps) {
    vector<future<void>> threads;
    threads.reserve(num_threads * inner_reps);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < num_threads * inner_reps; i++) {
        threads.push_back(hpx::async(delay, delay_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//MASTER TASK BUSY SLAVES
future<void> master_busy_thread(int thread_id, int inner_reps) {
    vector<future<void>> tasks;
    for(int i = 0; i < inner_reps; i++) {
        if(thread_id == 0) {
            tasks.push_back(hpx::async(delay, delay_reps));
        } else {
            delay( inner_reps );
        }
    }
    return hpx::when_all(tasks);
}

uint64_t testMasterTaskGenerationWithBusySlaves(int num_threads, int inner_reps)  {
    vector<future<void>> threads;
    threads.reserve(num_threads);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(master_busy_thread, i, inner_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//TASK WAIT
void spawn_and_wait(int inner_reps) {
    for(int i = 0; i < inner_reps; i++) {
        auto task = hpx::async(delay, delay_reps);
        task.wait();
    }
}
// this is very similar to PARALLEL TASK, only with taskwaits in each thread
// this is probably going to be slower, though.
uint64_t testTaskWait(int num_threads, int inner_reps){
    vector<future<void>> threads;
    threads.reserve(num_threads);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(spawn_and_wait, inner_reps));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//NESTED TASK
void spawn_nested_tasks_wait(int num_threads, int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps);
    for(int i = 0; i < inner_reps; i++) {
        tasks.push_back(hpx::async(spawn_tasks_wait, num_threads ));
    }
    hpx::wait_all(tasks);
}
uint64_t testNestedTaskGeneration(int num_threads, int inner_reps) {
    vector<future<void>> threads;
    threads.reserve(num_threads);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(spawn_nested_tasks_wait, num_threads, inner_reps/num_threads));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//NESTED MASTER TASK
uint64_t testNestedMasterTaskGeneration(int num_threads, int inner_reps) {
    vector<future<void>> threads;
    threads.reserve(inner_reps);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < inner_reps; i++) {
        threads.push_back(hpx::async(spawn_tasks_wait, num_threads ));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//BRANCH TASK TREE 
future<void> branch2(int tree_level);

future<void> branch1(int tree_level) {
    future<void> f = hpx::async(branch2, tree_level);
    delay(delay_reps);
    return f;
}

future<void> branch2(int tree_level) {
    vector<future<void>> tasks;
    tasks.push_back( hpx::async( delay, delay_reps ));
    if(tree_level == 1 ) {
        return std::move(tasks[0]);
    }
    tasks.push_back( hpx::async( branch2, tree_level-1 ));
    tasks.push_back( branch2( tree_level-1 ));
    return hpx::when_all(tasks);
}
future<void> branch_thread_func(int inner_reps) {
    vector<future<void>> tasks;
    for(int i = 0; i < (inner_reps >> DEPTH); i++) {
        tasks.push_back(hpx::async(branch1, DEPTH));
    }
    return hpx::when_all(tasks);
}
uint64_t testBranchTaskGeneration(int num_threads, int inner_reps) {
    vector<future<void>> threads;
    threads.reserve(num_threads);
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(branch_thread_func( inner_reps ));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

//LEAF TASK TREE
future<void> leaf_task_tree(int tree_level) {
    vector<future<void>> tasks;
    if( tree_level == 1 ) {
        tasks.push_back( hpx::async(delay, delay_reps));
        tasks.push_back( hpx::async(delay, delay_reps));
    } else {
        tasks.push_back( hpx::async(leaf_task_tree, tree_level-1));
        tasks.push_back( hpx::async(leaf_task_tree, tree_level-1));
    } 
    return hpx::when_all(tasks);
}
future<void> leaf_thread_func(int inner_reps) {
    vector<future<void>> tasks;
    tasks.reserve(inner_reps >> DEPTH);
    for(int i = 0; i < (inner_reps >> DEPTH); i++) {
        tasks.push_back(hpx::async(leaf_task_tree, DEPTH));
    }
    return hpx::when_all(tasks);
}
uint64_t testLeafTaskGeneration(int num_threads, int inner_reps) {
    uint64_t start = hpx::util::high_resolution_clock::now();
    vector<future<void>> threads;
    threads.reserve(num_threads);
    for(int i = 0; i < num_threads; i++) {
        threads.push_back(hpx::async(leaf_thread_func, inner_reps ));
    }
    hpx::wait_all(threads);
    return hpx::util::high_resolution_clock::now() - start;
}

void print_tasks(int num_threads){
    /*
    hpx::performance_counters::performance_counter count(
            "/threadqueue{locality#0/total}/length");
    int Ntasks = count.get_value<int>().get();
    cout << "num active tasks : " << Ntasks << endl;
    
    //TODO: show the number for each test, not the current accumulation
    for(int i = 0; i < num_threads; i++) {
        std::string name = "/threads{locality#0/worker-thread#";
        name += std::to_string(i);
        name += "}/count/cumulative";
        hpx::performance_counters::performance_counter completed(name);
        //"/threads{locality#0/total}/count/cumulative");
        int Ncompleted = completed.get_value<int>().get();
        completed.reset();
        cout << Ncompleted << " completed hpx threads on thread " << i << endl;
    }
    */
}

void print_time(std::vector<double> time, std::string name) {
    print_tasks(hpx::get_os_thread_count());
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
         << (total / time.size()) << " - " << min << " - " << max << endl;
    
}

void print_delay_time(){
    uint64_t start = hpx::util::high_resolution_clock::now();
    for(int i = 0; i < 1000; i++) {
        delay(delay_reps);
    }
    uint64_t total = hpx::util::high_resolution_clock::now() - start;
    double average = (double)total / (double)1000.0;
    cout << "total delay time is: " << average << " nanoseconds)" << endl;
}

int hpx_main(boost::program_options::variables_map& vm) {
    int num_threads = hpx::get_os_thread_count();

    int reps = vm["reps"].as<int>();
    int inner_reps = vm["inner_reps"].as<int>();
    delay_reps= vm["delay_reps"].as<int>();
    vector<double> time(reps);

    print_delay_time();
    print_tasks(num_threads);

    task_counter = 0;
    for(int i = 0; i < reps; i++) {
        time[i] = testParallelTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "PARALLEL TASK");//20
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testMasterTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "MASTER TASK");//20
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testMasterTaskGenerationWithBusySlaves(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "MASTER TASK BUSY SLAVES");//1280 / 640 (hpxMP)
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testTaskWait(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "TASK WAIT");//1280 / 80 (hpxMP)
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testNestedTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "NESTED TASK");//2560 / 80 (hpxMP)
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testNestedMasterTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "NESTED MASTER TASK");//2560 / 160 (hpxMP)
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testBranchTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "BRANCH TASK TREE");//5120 / 160 (hpxMP)
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;

    for(int i = 0; i < reps; i++) {
        time[i] = testLeafTaskGeneration(num_threads, inner_reps) / (double)inner_reps;
    }
    print_time(time, "LEAF TASK TREE");//5120 / 160 (hpxMP)
    cout << "task counter = " << task_counter << endl;
    task_counter = 0;


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
        ( "inner_reps", value<int>()->default_value(1024),
          "corresponds to the number of tasks spawned, default 1024")
        ( "delay_reps", value<int>()->default_value(10000),
          "number of iterations in delay function (0-?, default: 10000)") ;

    return hpx::init(desc_commandline, argc, argv, cfg);
}

