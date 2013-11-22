#include <stdio.h>

int main() {
    int x=0;
#pragma omp parallel
    {
#pragma omp single
        {
#pragma omp task
            {
                x = x + 1;
                printf("x1 = %d\n", x);
            }
#pragma omp taskwait
#pragma omp task
            {
                x = x + 1;
                printf("x2 = %d\n", x);
            }
        }
    }
    printf("x = %d\n", x);
    return 0;
}
