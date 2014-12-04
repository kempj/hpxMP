#include <stdio.h>

int main()
{
    int x = 42;
    int i;
#pragma omp parallel 
    {
#pragma omp for
        for(i = 0; i < 4;i++) {
            if(i == 0)
                x++;
        }
#pragma omp barrier
        printf("x = %d\n", x);
    }
}
