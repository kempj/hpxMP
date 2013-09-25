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
    printf("Third loop: stride of 2, 0-9\n");
#pragma omp parallel for 
    for(i = 0; i < 10; i += 2) 
    {
        printf("Hello world %d\n",i);
    }
    printf("Fourth loop: stride of 2, 0-10\n");
#pragma omp parallel for 
    for(i = 0; i <= 10; i += 2) 
    {
        printf("Hello world %d\n",i);
    }
    printf("Fifth loop: stride of 1, -5-4\n");
#pragma omp parallel for 
    for(i = -5; i < 5; i++) 
    {
        printf("Hello world %d\n",i);
    }
    printf("Sixth loop: stride of 2, -5-4\n");
#pragma omp parallel for 
    for(i = -5; i < 5; i+=2) 
    {
        printf("Hello world %d\n",i);
    }
    //stride larger than range
    //
    return 0;
}
