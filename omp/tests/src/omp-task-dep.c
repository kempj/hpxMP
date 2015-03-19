#include <stdio.h>

int foo() {
    return 42;
}

int bar(int in) {
    return in + 11;
}

int main(int argc, char **argv) {
    int x = 5, y = 1, z = 0;
    printf("&x = %p\n", &x);
#pragma omp parallel
    {
#pragma omp single
        {
#pragma omp task depend(out: x)
            x = foo();

#pragma omp task depend(in: x) depend(out : y)
            y = bar(x);

#pragma omp task depend(in: y)
            z = bar(y);
        }
    }
    printf("x = %d\n",x);
    printf("y = %d\n",y);
    printf("z = %d\n",z);
}
