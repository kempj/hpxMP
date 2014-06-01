#include <stdio.h>
#include <stdlib.h>

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
        printf("Dynamic Hello World %d\n", i);
    }
    

    return 0;
}
