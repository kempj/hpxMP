#include <hpx/hpx_init.hpp>
#include <hpx/hpx.hpp>
#include <unistd.h>

using hpx::lcos::shared_future;

void delay(int seconds) {
    sleep(seconds);
}

void wait_delay(int seconds, shared_future<void> f1){//, future<void> f2, future<void> f3) {
    sleep(seconds);
    hpx::async(delay, 2);
    f1.wait();
    //f2.wait();
    //f3.wait();
    sleep(seconds);
}

int hpx_main()
{

    shared_future<void> f1 = hpx::async(delay, 2);
    //future<void> f2 = hpx::async(delay, 2);
    //future<void> f3 = hpx::async(delay, 2);
    shared_future<void> f4 = hpx::async(wait_delay, 1, f1);//, f2, f3);
    return hpx::finalize();
}


int main(int argc, char **argv)
{

    using namespace boost::assign;

    std::vector<std::string> cfg;
    cfg += "hpx.os_threads=2";// + 
    //    boost::lexical_cast<std::string>(hpx::threads::hardware_concurrency());

    return hpx::init(argc, argv, cfg);

}
