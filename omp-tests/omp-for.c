#include <stdio.h>

int main() {
    int i;

#pragma omp parallel for
    for(i = 0; i < 11; i++) 
    {
        printf("Hello World %d\n", i);
    }
    
    printf("second loop: separate parallel and for pragmas\n");
#pragma omp parallel 
    {
#pragma omp for
        for(i = 0; i < 11; i++) 
        {
            printf("Hello World %d\n", i);
        }
    }
    return 0;
}
