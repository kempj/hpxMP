#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <boost/format.hpp>

#include <chrono>

using hpx::lcos::shared_future;
using hpx::lcos::future;
using std::vector;
using std::cout;
using std::endl;
using std::chrono::high_resolution_clock;
using time_point = std::chrono::system_clock::time_point;

void print_time(time_point t1, time_point t2, std::string name) {
     auto time = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
     cout << name << ": " << time << " microseconds" << endl;
}

struct block {
    double *ptr;
    int stride;
    int row;
    int col;
    int width;
    int height;
};

block rec_mult(block A, block B, block C) {
    //termination condition
    //sub divide
    return C;
}

int hpx_main(boost::program_options::variables_map& vm) {
    return hpx::finalize();
}

int main(int argc, char ** argv) {

    hpx::init(argc, argv);

    return 0;
}
