/*
 * fibonnaci test
 * select tasking or sequential implementation on command line
 *
 * Author: Deepak
 */

#include <sys/time.h>
#include <stdio.h>

long fib1(int k);
long fib2(int k);

int num_tasks = 0;


int main(int argc, char* argv[])
{
    struct timeval t1;
    struct timeval t2;

    int input;
    long s,u;
    long f;
    double m;
    int nt;
    int mode = 0;

    if (argc != 4 && argc != 3) {
        fprintf(stderr, "Usage: ./fib task|seq <input> <nt>\n");
        return 1;
    }

    nt = omp_get_max_threads();

    input = atoi(argv[2]);
    if(argc == 4)
      nt = atoi(argv[3]);

    if (!strncmp(argv[1], "task", 4))
        mode = 1;
    else if (!strncmp(argv[1], "seq", 3)) {
        mode = 0;
        nt = 1;
    }
    else {
        fprintf(stderr, "Usage: ./fib task|seq <input> <nt>\n");
        return 1;
    }


    gettimeofday(&t1, NULL);

#pragma omp parallel num_threads(nt)
    {
#pragma omp master
        {
#pragma omp task shared(f)
            {
                f = fib1(input);
//                printf("1fib(%d) = %d\n",input, f);
            }
//            printf("2fib(%d) = %d\n",input, f);
        }
//        printf("3fib(%d) = %d\n",input, f);
    }


    gettimeofday(&t2, NULL);
    printf("fib(%d) = %d\n",input, f);

    s = t2.tv_sec - t1.tv_sec;
    u = t2.tv_usec - t1.tv_usec;
    m = (s*1000 + u/1000.0)  + 0.5;
    printf("# tasks: %d\n", num_tasks);
    printf("time = %.2lfms\n", m );

}


long fib1(int k)
{
    long p2,p1;

    if (k == 2) return 1;
    if (k < 2){
        return k;
    }


#pragma omp task shared(p2) //if(k > 10)
    {
        p2 = fib1(k-2);
    }

#pragma omp task shared(p1) //if(k > 10)
    {
        p1 = fib1(k-1);
    }

#pragma omp atomic
    num_tasks += 2;

#pragma omp taskwait
    return (p2+p1);
}

long fib2(int k)
{
    int i;
    long p2,p1,p0;

    if (k == 2) return 1;
    if (k < 2) return k;

    printf("0 1 1 ");

    p2 = 1;
    p1 = 2;
    p0 = p1;
    for (i = 3; i < k; i++) {
        p0 = p2+p1;
        p2 = p1;
        p1 = p0;
        printf("%d ", p0);
    }

    printf("\n");

    return p0;
}
