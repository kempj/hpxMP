#include "loop_data.h"
#include "hpx_runtime.h"

mtx_ptr loop_mtx;
extern boost::shared_ptr<hpx_runtime> hpx_backend;

loop_data::loop_data(){
    loop_mtx.reset(new mutex_type);
    num_threads = hpx_backend->get_num_threads();
}

void loop_data::yield(){
    hpx::this_thread::yield();
}

void loop_data::lock(){
    loop_mtx->lock();
}

void loop_data::unlock(){
    loop_mtx->unlock();
}
