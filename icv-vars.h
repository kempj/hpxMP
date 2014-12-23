
#include <limits>

struct omp_device_icv {
    //Device scoped:
    int def_sched{0};  //static schedule
    //stacksize
    //wait_policy //active
    int max_active_levels{std::numeric_limits<int>::max()};
    bool cancel{false};
    //int stacksize_var; //-Ihpx.stacks.small_size=... (use hex numbers)
        //http://stellar-group.github.io/hpx/docs/html/hpx/manual/init/configuration/config_defaults.html
};

struct omp_icv {
    //Data environment scoped:
    bool dyn{false};
    bool nest{false};
    int nthreads;
    int run_sched{0};//static schedule
    bool bind{false};
    int thread_limit{std::numeric_limits<int>::max()};
    int active_levels{0};
    int levels{0};
    int default_device{0};
    //scoped to implicit task
    //int place_partition;//set to all threads
    omp_device_icv *device;
};

