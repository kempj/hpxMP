#include <hpx/hpx_init.hpp>
#include <hpx/hpx.hpp>


int hpx_main()
{

    return hpx::finalize();
}


int main(int argc, char **argv)
{

    return hpx::init(argc, argv);
}
