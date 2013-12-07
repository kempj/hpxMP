
#include <hpx/hpx.hpp>
#include <hpx/hpx_fwd.hpp>
#include <hpx/hpx_start.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/lcos/local/barrier.hpp>
#include <hpx/util/static.hpp>
#include <hpx/include/lcos.hpp>

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>


typedef void *frame_pointer_t;
typedef int omp_tid;
typedef void (*omp_micro)(int , frame_pointer_t);

typedef void (*omp_task_func)(void *firstprivates, void *fp);

typedef hpx::lcos::local::spinlock mutex_type;

using hpx::lcos::local::barrier;
using hpx::lcos::future;
using std::cout;
using std::endl;
using std::vector;

struct thread_data {
    int thread_num;
    vector<future<void>> task_handles;
};
class hpx_runtime {
    public:
        void init(int num_threads);
        void fork(int num_threads, omp_micro micro_task, frame_pointer_t fp);
        
    private:
        bool hpx_initialized = false;
        mutex_type single_mtx;
        mutex_type init_mtx;
        barrier *globalBarrier;
        int num_threads;
};

