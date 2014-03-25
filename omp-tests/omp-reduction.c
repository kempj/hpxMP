#include <stdio.h>
int main() {
    int i, num_steps=15, x, sum = 0, pi;

#pragma omp parallel for reduction (+:sum)
    for(i=1;i<=num_steps;i++)
    {   
        sum += 1.0+i*i;
        printf("i = %d, sum = %d, thread = %d\n", i, sum, omp_get_thread_num());
    }
    printf("sum = %d\n", sum);
    return 0;
}
