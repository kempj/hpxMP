#include <sys/time.h>
#include <stdio.h>
#include "hpxMP.h"


struct task_args_fib {
    int k;
};

long fib1(int k);

static void fib1_1(task_args_fib *taskargs, void *fake_fp);
static void fib1_2(task_args_fib *taskargs, void *fake_fp);

int cutoff = 26;

static void parallel_region_func(int gtid_a,void *fake_fp);
static void task_func(void *unused_taskargs, void *fake_fp);

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

static void task_func(void  *unused_taskargs, void *fake_fp)
{
    long_int_struct *fp = (long_int_struct *)fake_fp;
    fp->result = fib1(fp->input);
    __ompc_task_exit();
    return;
} 

long fib1(int k)
{
    long p2, p1, i;
    struct task_args_fib * taskarg1;
    struct task_args_fib * taskarg2;
    int is_deferred1, is_deferred2;
    struct long_int_struct fake_fp1, fake_fp2;

    if (k == 2){ return 1; }
    if (k < 2) { return k; }

    is_deferred1 = __ompc_task_will_defer(k > cutoff);
    if(is_deferred1) {
        __ompc_task_firstprivates_alloc(&taskarg1, 4);
        (taskarg1) -> k = k;
    } else {
        taskarg1 = (struct task_args_fib *)(0ULL);
    }

    __ompc_task_create(&fib1_1,(void*) &fake_fp1, taskarg1, is_deferred1, 0, 0);

    is_deferred2 = __ompc_task_will_defer(k > cutoff);
    if(is_deferred2) {
        __ompc_task_firstprivates_alloc(&taskarg2, 4);
        (taskarg2) -> k = k;
    } else {
        taskarg2 = (struct task_args_fib *)(0ULL);
    }
    __ompc_task_create(&fib1_2, (void*)&fake_fp2, taskarg2, is_deferred2, 0, 0);
    __ompc_task_wait();
    p1 = fake_fp1.result;
    p2 = fake_fp2.result;
    return p2 + p1;
} 

static void fib1_1(struct task_args_fib *taskargs, void *fake_fp)
{
    long_int_struct *fp = (long_int_struct *)fake_fp;
    //struct task_args_fib tmp_k;
    int local_k;

    if(taskargs)
    {
        local_k = (taskargs)->k;
//        local_k = *(int *) & tmp_k;
//    } else {
//        local_k = *((int *) _temp___slink_sym8 + -4LL);
//    }
    fp->result = fib1(local_k + -2);
    __ompc_task_exit();
    if(taskargs) {
        __ompc_task_firstprivates_free(taskargs);
    }
    return;
}


static void fib1_2(task_args_fib *taskargs, void *fake_fp)
{

    register _UINT64 _w2c_reg6;
    register _UINT64 _w2c_reg5;
    struct task_args_fib _tmp0;
    register long _w2c___comma;
    register int _w2c_reg1;
    _UINT64 _temp___slink_sym9;
    int local_k;

    _temp___slink_sym9 = _w2c_reg6;
    taskargs = (struct task_args_fib *)(_w2c_reg5);
    fake_fp = _w2c_reg6;
    _temp___slink_sym9 = fake_fp;
    if(!(taskargs))
        goto _1539;
    _tmp0 = (struct task_args_fib)(taskargs) -> k;
    local_k = *(int *) & _tmp0;
    goto _1283;
_1539 :;
       local_k = *((int *) _temp___slink_sym9 + -4LL);
_1283 :;
       _w2c_reg5 = local_k + -1;
       _w2c___comma = fib1(local_k + -1);
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

