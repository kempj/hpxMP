#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main() {
    int i,j;


#pragma omp parallel for schedule(static)
    for(i = 0; i < 11; i++) 
    {
        printf("Static Hello World %d\n", i);
    }

#pragma omp parallel for schedule(static, 1)
    for(i = 0; i < 11; i++) 
    {
        printf("Static1 Hello World %d\n", i);
    }

#pragma omp parallel for schedule(static, 2)
    for(i = 0; i < 11; i++) 
    {
        printf("Static2 Hello World %d\n", i);
    }

#pragma omp parallel for schedule(dynamic)
    for(i = 0; i < 16; i++) 
    {
        printf("Thread %d: Dynamic Hello World %d\n", omp_get_thread_num(), i);
    }
    

    return 0;
}
