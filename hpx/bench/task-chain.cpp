
#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <hpx/include/util.hpp>

#include <boost/assign/std/vector.hpp>
#include <boost/cstdint.hpp>
#include <boost/format.hpp>

using hpx::lcos::future;

using std::vector;
using std::cout;
using std::endl;


int hpx_main(boost::program_options::variables_map& vm) {

    int length = vm["length"].as<int>();
    int num_threads = hpx::get_os_thread_count();

    for(int i = 0; i < length; i++) {

    }
    return hpx::finalize();
}

int main(int argc, char ** argv) {


    boost::program_options::options_description
       desc_commandline("Usage: " HPX_APPLICATION_STRING " [options]");

    using boost::program_options::value;
    desc_commandline.add_options()
        ( "length", value<int>()->default_value(20),
          "number of times to repeat the benchmark") ;

    return hpx::init(desc_commandline, argc, argv);
}
