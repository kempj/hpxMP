#include "stdio.h"

int main()
{
#pragma omp parallel
    {
    printf("hello world 1\n");
#pragma omp barrier
    printf("hello world 2\n");
    }
    return 0;
}
