
#include <stdio.h>

int main() {
    int x = 0;

#pragma omp parallel
    {
        x = 3;//omp_get_thread_num();
        printf("x = %d\n",x);
#pragma omp critical
        x = 42;
        printf("\tx = %d\n",x);
#pragma omp critical
        x = 43;
        printf("\tx = %d\n",x);
    }
    return 0;
}
