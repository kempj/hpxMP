#include <stdio.h>

int x;
#pragma omp threadprivate(x)

int main() {

#pragma omp parallel
    {
        x = omp_get_thread_num();
    }
/*
#pragma omp parallel
    {
        printf("%d %d\n", x, omp_get_thread_num());

    }
#pragma omp parallel
    {
        printf("%d %d\n", x, omp_get_thread_num());

    }
#pragma omp parallel
    {
        printf("%d %d\n", x, omp_get_thread_num());

    }*/
#pragma omp parallel
    {
        printf("%d %d\n", x, omp_get_thread_num());

    }
}
