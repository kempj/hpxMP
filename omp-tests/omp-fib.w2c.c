#include <sys/time.h>
#include <stdio.h>
#include "hpxMP.h"

long fib1(int k);

static void fib1_1(int taskargs, void *fake_fp);
static void fib1_2(int taskargs, void *fake_fp);

int cutoff = 26;

static void parallel_region_func(int gtid_a,void *fake_fp);
static void task_func(int taskargs, void *fake_fp);

struct task_args_fib {
    int k;
};

struct long_int_struct {
    long result;
    int input;
};

int main(int argc, char ** argv)
{
    struct timeval t1;
    struct timeval t2;
    int input;
    long s, u, f;
    double m;
    long_int_struct fake_fp;

    if (argc != 2 && argc != 3) {
        fprintf(stderr, "Usage: ./fib <input> <cutoff>\n");
        return 1;
    }

    input = atoi(argv[1]);
    fake_fp.input = input;
    if(argc == 3)
        cutoff= atoi(argv[2]);

    gettimeofday(&t1, NULL);

    __ompc_fork(0, &parallel_region_func, &fake_fp);

    gettimeofday(&t2, (struct timezone *) 0ULL);
    printf("fib(%d)= %d\n", input, fake_fp.result);
    s = (t2).tv_sec - (t1).tv_sec;
    u = (t2).tv_usec - (t1).tv_usec;
    m = ((double)((s * 1000LL)) + ((double)(u) / 1.0e+03)) + 5.0e-01;
    printf("cutoff = %d\n", cutoff);
    printf("time = %.2lfms\n", m);
    return 0;
} 


static void parallel_region_func(int gtid_a, void *fake_fp)
{
    int _w2c_mp_is_master;

    _w2c_mp_is_master = __ompc_master(gtid_a);
    if(_w2c_mp_is_master == 1)
        __ompc_task_create(&task_func, fake_fp, 0ULL, 1, 0, 1);
    __ompc_end_master(gtid_a);
    __ompc_task_exit();
    return;
} 


static void task_func(int taskargs, void *fake_fp)
{
    long_int_struct *fp = (long_int_struct *)fake_fp;
    //_temp___slink_sym3 = fake_fp;
    //* (*((long **) _temp___slink_sym3 + -4LL) + -8LL) = fib1(*(*((int **) _temp___slink_sym3 + -4LL) + -7LL));
    fp->result = fib1(fp->input);
    __ompc_task_exit();
    return;
} 

long fib1(int k)
{
    long p2, p1, i;
    int xpragma6;
    int xpragma7;
    struct task_args_fib * taskarg1;
    struct task_args_fib * taskarg2;
    int is_deferred1, is_deferred2;
    struct long_int_struct fake_fp1, fake_fp2;

    if (k == 2)i{ return 1; }
    if (k < 2){ return k; }

    xpragma6 = k > cutoff;
    is_deferred1 = __ompc_task_will_defer(xpragma6);
    if(is_deferred1) {
        __ompc_task_firstprivates_alloc(&taskarg1, 4);
        (taskarg1) -> k = k;
    } else {
        taskarg1 = (struct task_args_fib *)(0ULL);
    }

    __ompc_task_create(&fib1_1,(void*) &fake_fp1, taskarg1, xpragma6, 0, 0);

    xpragma7 = k > cutoff;

    is_deferred2 = __ompc_task_will_defer(xpragma7);
    if((is_deferred2))
    {
        __ompc_task_firstprivates_alloc(&taskarg2, 4);
        (taskarg2) -> k = k;
    } else {

       taskarg2 = (struct task_args_fib *)(0ULL);
    }
       __ompc_task_create(&fib1_2, (void*)&fake_fp2, taskarg2, xpragma7, 0, 0);
       __ompc_task_wait();
       return p2 + p1;
} 


static void fib1_1(int taskargs, void *fake_fp)
{

    struct task_argsfib1_1 _tmp0;
    _UINT64 _temp___slink_sym8;
    int __mplocal_k;

    /*Begin_of_nested_PU(s)*/

    _temp___slink_sym8 = _w2c_reg6;
    taskargs = (struct task_argsfib1_1 *)(_w2c_reg5);
    fake_fp = _w2c_reg6;
    _temp___slink_sym8 = fake_fp;
    if(!(taskargs))
        goto _1539;
    _tmp0 = (struct task_argsfib1_1)(taskargs) -> k;
    __mplocal_k = *(int *) & _tmp0;
    goto _1283;
_1539 :;
       __mplocal_k = *((int *) _temp___slink_sym8 + -4LL);
_1283 :;
       _w2c_reg5 = __mplocal_k + -2;
       _w2c___comma = fib1(__mplocal_k + -2);
       * ((long *) _temp___slink_sym8 + -8LL) = _w2c___comma;
       _w2c_reg1 = 0;
       __ompc_task_exit();
       if(!(taskargs))
           goto _1795;
       _w2c_reg5 = (_UINT64)(taskargs);
       _w2c_reg1 = 0;
       __ompc_task_firstprivates_free(taskargs);
_1795 :;
       return;
} /* fib1_1 */


static void fib1_2(taskargs, fake_fp)
    struct task_argsfib1_2 * taskargs;
    _UINT64 fake_fp;
{

    register _UINT64 _w2c_reg6;
    register _UINT64 _w2c_reg5;
    struct task_argsfib1_2 _tmp0;
    register long _w2c___comma;
    register int _w2c_reg1;
    _UINT64 _temp___slink_sym9;
    int __mplocal_k;

    _temp___slink_sym9 = _w2c_reg6;
    taskargs = (struct task_argsfib1_2 *)(_w2c_reg5);
    fake_fp = _w2c_reg6;
    _temp___slink_sym9 = fake_fp;
    if(!(taskargs))
        goto _1539;
    _tmp0 = (struct task_argsfib1_2)(taskargs) -> k;
    __mplocal_k = *(int *) & _tmp0;
    goto _1283;
_1539 :;
       __mplocal_k = *((int *) _temp___slink_sym9 + -4LL);
_1283 :;
       _w2c_reg5 = __mplocal_k + -1;
       _w2c___comma = fib1(__mplocal_k + -1);
       * ((long *) _temp___slink_sym9 + -9LL) = _w2c___comma;
       _w2c_reg1 = 0;
       __ompc_task_exit();
       if(!(taskargs))
           goto _1795;
       _w2c_reg5 = (_UINT64)(taskargs);
       _w2c_reg1 = 0;
       __ompc_task_firstprivates_free(taskargs);
_1795 :;
       return;
}

