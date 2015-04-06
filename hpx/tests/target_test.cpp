#include <iostream>

void foo(int *in_vals, int size, int *out_vals)
{
    for(int i = 0; i < size; i++) {
        out_vals[i] = in_vals[i] + 1;
    }
}

int main(int argc, char** argv) 
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

}
