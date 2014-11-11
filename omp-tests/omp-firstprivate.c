#include <omp.h>
#include <stdio.h>
#include <math.h>


void test_omp_for_firstprivate () {
    int sum0 = 10;

#pragma omp parallel
    {
        sum0 += omp_get_thread_num();
        int i;
#pragma omp for firstprivate(sum0)
        for(i = 1; i <= 5; i++) {
            sum0 = sum0 + 1;
            printf("sum0 (%d) = %d, at %p\n", omp_get_thread_num(), sum0, &sum0);
        }
        printf("sum0 = %d at %p\n", sum0, &sum0);
    }
    printf("final sum0 = %d %p\n", sum0, &sum0);
}

int main()
{

    test_omp_for_firstprivate();

    return 0;
}
