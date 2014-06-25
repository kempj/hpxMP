#include <sys/time.h>
#include <stdio.h>
#include "hpxMP.h"

long fib1(int k);

int cutoff = 26;

static void __omprg_main_1(int __ompv_gtid_a,void *fake_fp);
static void __omprg___omprg_main_1_1(int __ompv_taskargs, void *fake_fp);

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

    __ompc_fork(0, &__omprg_main_1, &fake_fp);

    gettimeofday(&t2, (struct timezone *) 0ULL);
    printf("fib(%d) = %d\n", input, fake_fp[0]);
    s = (t2).tv_sec - (t1).tv_sec;
    u = (t2).tv_usec - (t1).tv_usec;
    m = ((double)((s * 1000LL)) + ((double)(u) / 1.0e+03)) + 5.0e-01;
    printf("cutoff = %d\n", cutoff);
    printf("time = %.2lfms\n", m);
    return 0;
} 


static void __omprg_main_1(int __ompv_gtid_a, void *fake_fp)
{
    int _w2c_mp_is_master;

    _w2c_mp_is_master = __ompc_master(__ompv_gtid_a);
    if(_w2c_mp_is_master == 1)
        __ompc_task_create(&__omprg___omprg_main_1_1, fake_fp, 0ULL, 1, 0, 1);
    __ompc_end_master(__ompv_gtid_a);
    __ompc_task_exit();
    return;
} 


static void __omprg___omprg_main_1_1(int __ompv_taskargs, void *fake_fp)
{
    //_temp___slink_sym3 = fake_fp;
    //* (*((long **) _temp___slink_sym3 + -4LL) + -8LL) = fib1(*(*((int **) _temp___slink_sym3 + -4LL) + -7LL));
    fake_fp.output = fib1(fake_fp.input);
    __ompc_task_exit();
    return;
} 

long fib1(int k)
{
    long p2, p1;
    int _temp__mp_xpragma6;
    int _temp__mp_xpragma7;
    //struct task_args__omprg_fib1_1 * __ompv_taskarg1;
    //struct task_args__omprg_fib1_2 * __ompv_taskarg2;
    int is_deferred;

    if (k == 2) return 1;
    if (k < 2){
        return k;
    }
       _temp__mp_xpragma6 = k > cutoff;
       is_deferred = __ompc_task_will_defer(_temp__mp_xpragma6);
       if(is_deferred) {
           __ompc_task_firstprivates_alloc(&__ompv_taskarg1, 4);
           (__ompv_taskarg1) -> k = k;
       } else {
       __ompv_taskarg1 = (struct task_args__omprg_fib1_1 *)(0ULL);
       }

       __ompc_task_create(&__omprg_fib1_1, _w2c_reg3, __ompv_taskarg1, _temp__mp_xpragma6, 0, 0);

       _temp__mp_xpragma7 = k > cutoff;

       _w2c___ompv_task_is_deferred0 = __ompc_task_will_defer(_temp__mp_xpragma7);
       if(!(_w2c___ompv_task_is_deferred0))
           goto _2562;
       __ompc_task_firstprivates_alloc(&__ompv_taskarg2, 4);
       (__ompv_taskarg2) -> k = k;
       goto _2306;
_2562 :;
       __ompv_taskarg2 = (struct task_args__omprg_fib1_2 *)(0ULL);
_2306 :;
       __ompc_task_create(&__omprg_fib1_2, _w2c_reg3, __ompv_taskarg2, _temp__mp_xpragma7, 0, 0);
       __ompc_task_wait();
       return p2 + p1;
} /* fib1 */


static void __omprg_fib1_1(int __ompv_taskargs, void *fake_fp)
{

    struct task_args__omprg_fib1_1 _tmp0;
    _UINT64 _temp___slink_sym8;
    int __mplocal_k;

    /*Begin_of_nested_PU(s)*/

    _temp___slink_sym8 = _w2c_reg6;
    __ompv_taskargs = (struct task_args__omprg_fib1_1 *)(_w2c_reg5);
    fake_fp = _w2c_reg6;
    _temp___slink_sym8 = fake_fp;
    if(!(__ompv_taskargs))
        goto _1539;
    _tmp0 = (struct task_args__omprg_fib1_1)(__ompv_taskargs) -> k;
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
       if(!(__ompv_taskargs))
           goto _1795;
       _w2c_reg5 = (_UINT64)(__ompv_taskargs);
       _w2c_reg1 = 0;
       __ompc_task_firstprivates_free(__ompv_taskargs);
_1795 :;
       return;
} /* __omprg_fib1_1 */


static void __omprg_fib1_2(__ompv_taskargs, fake_fp)
    struct task_args__omprg_fib1_2 * __ompv_taskargs;
    _UINT64 fake_fp;
{

    register _UINT64 _w2c_reg6;
    register _UINT64 _w2c_reg5;
    struct task_args__omprg_fib1_2 _tmp0;
    register long _w2c___comma;
    register int _w2c_reg1;
    _UINT64 _temp___slink_sym9;
    int __mplocal_k;

    /*Begin_of_nested_PU(s)*/

    _temp___slink_sym9 = _w2c_reg6;
    __ompv_taskargs = (struct task_args__omprg_fib1_2 *)(_w2c_reg5);
    fake_fp = _w2c_reg6;
    _temp___slink_sym9 = fake_fp;
    if(!(__ompv_taskargs))
        goto _1539;
    _tmp0 = (struct task_args__omprg_fib1_2)(__ompv_taskargs) -> k;
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
       if(!(__ompv_taskargs))
           goto _1795;
       _w2c_reg5 = (_UINT64)(__ompv_taskargs);
       _w2c_reg1 = 0;
       __ompc_task_firstprivates_free(__ompv_taskargs);
_1795 :;
       return;
} /* __omprg_fib1_2 */

