#include <stdio.h>

int foo() {
    return 42;
}

int bar(int in) {
    return in + 11;
}

int main(int argc, char **argv) {
    int x = 0, y = 1;
#pragma omp parallel
    {
#pragma omp single
        {
#pragma omp task depend(out: x)
            x = foo();

#pragma omp task depend(in: x)
            y = bar(x);
        }
    }
    printf("y = %d\n",y);
}
