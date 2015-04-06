#include <iostream>
#include <hpx/hpx.hpp>

void foo(int *in_vals, int size, int *out_vals)
{
    for(int i = 0; i < size; i++) {
        out_vals[i] = in_vals[i] + 1;
    }
}

int hpx_main()
{
    const int size = 10;
    int in[size], out[size];
    for(int i = 0; i < size; i++) {
        in[i] = i;
    }

    foo(in, size, out);

    for(int i = 0; i < size; i++) {
        std::cout << out[i] << ", ";
    }
    std::cout << std::endl;

    return hpx::finalize();
}

int main(int argc, char** argv) 
{
    return hpx::init(argc, argv);
}
