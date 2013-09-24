#include <stdio.h>
#include <time.h>

int main() {
    struct timespec requestStart, requestEnd;
    clock_gettime(CLOCK_REALTIME, &requestStart);
#pragma omp parallel
    {
        printf("Hello World\n");
    }
    clock_gettime(CLOCK_REALTIME, &requestEnd);
    printf("Parallel region took %d second, and %d nanoseconds\n", requestEnd.tv_sec - requestStart.tv_sec, 
                                                                   requestEnd.tv_nsec - requestStart.tv_nsec);
#pragma omp parallel
    {
        printf("second parallel region\n");
    }
    clock_gettime(CLOCK_REALTIME, &requestEnd);
    printf("Total of both parallel regions took %d second, and %d nanoseconds\n", requestEnd.tv_sec - requestStart.tv_sec, 
                                                                   requestEnd.tv_nsec - requestStart.tv_nsec);
    return 0;
}
