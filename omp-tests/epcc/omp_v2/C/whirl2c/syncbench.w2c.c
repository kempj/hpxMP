/*******************************************************
 * C file translated from WHIRL Wed Nov  9 19:52:19 2005
 *******************************************************/

/* Include file-level type and variable decls */
#include "syncbench.w2c.h"


extern _INT32 main(
  _INT32 argv,
  _INT8 ** argc)
{
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _INT32 __comma;
  register _INT64 reg8;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_main1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 mp_is_master;
  register _INT32 __comma;
  _UINT64 __slink_sym_temp_0;
  _INT32 __ompv_temp_gtid;
  
  __slink_sym_temp_0 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  mp_is_master = __ompc_master(__ompv_temp_gtid);
  if(mp_is_master == 1)
  {
    __comma = omp_get_num_threads();
    nthreads = __comma;
  }
  return;
} /* __ompregion_main1 */
  
  __ompv_ok_to_fork = __ompc_can_fork();
  if(__ompv_ok_to_fork == 1)
  {
    __ompc_fork(0, &__ompregion_main1, reg7);
  }
  else
  {
    __ompv_gtid_s1 = __ompc_get_local_thread_num();
    __ompc_serialized_parallel();
    __comma = omp_get_num_threads();
    nthreads = __comma;
    __ompc_end_serialized_parallel();
  }
  printf(" Running OpenMP benchmark on %d thread(s)\n", nthreads);
  delaylength = 500;
  innerreps = 10000;
  refer();
  innerreps = 1000;
  testpr();
  testfor();
  testpfor();
  testbar();
  testsing();
  innerreps = 100000;
  testcrit();
  testlock();
  innerreps = 1000;
  testorder();
  innerreps = 100000;
  referatom();
  testatom();
  innerreps = 10000;
  referred();
  innerreps = 1000;
  testred();
  return reg8;
} /* main */




extern void refer()
{
  register _IEEE64 __comma;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing reference time 1\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      delay(delaylength);
      _2 :;
      j = j + 1;
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("Reference_time_1 =                        %f microseconds +/- %f\n", meantime, sd * 1.96);
  reftime = meantime;
  refsd = sd;
  return;
} /* refer */


extern void referatom()
{
  register _IEEE64 __comma;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _IEEE32 aaaa;
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing reference time 2\n");
  k = 0;
  while(k <= 20)
  {
    aaaa = 0.0F;
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      aaaa = aaaa + 1.0F;
      _2 :;
      j = j + 1;
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    if(aaaa < 0.0F)
    {
      printf("%f\n", (_IEEE64)(aaaa));
    }
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("Reference_time_2 =                        %f microseconds +/- %f\n", meantime, sd * 1.96);
  reftime = meantime;
  refsd = sd;
  return;
} /* referatom */


extern void referred()
{
  register _IEEE64 __comma;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 aaaa;
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing reference time 3\n");
  k = 0;
  while(k <= 20)
  {
    aaaa = 0;
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      delay(delaylength);
      aaaa = aaaa + 1;
      _2 :;
      j = j + 1;
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    if(aaaa < 0)
    {
      printf("%d\n", aaaa);
    }
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("Reference_time_3 =                        %f microseconds +/- %f\n", meantime, sd * 1.96);
  reftime = meantime;
  refsd = sd;
  return;
} /* referred */


extern void testpr()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testpr1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  _UINT64 __slink_sym_temp_2;
  _INT32 __ompv_temp_gtid;
  
  __slink_sym_temp_2 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  delay(delaylength);
  return;
} /* __ompregion_testpr1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing PARALLEL time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      __ompv_ok_to_fork = __ompc_can_fork();
      if(__ompv_ok_to_fork == 1)
      {
        __ompc_fork(0, &__ompregion_testpr1, reg7);
      }
      else
      {
        __ompv_gtid_s1 = __ompc_get_local_thread_num();
        __ompc_serialized_parallel();
        delay(delaylength);
        __ompc_end_serialized_parallel();
      }
      _2 :;
      j = j + 1;
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("PARALLEL time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("PARALLEL overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testpr */




extern void testfor()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 i;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testfor1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 temp_limit;
  _UINT64 __slink_sym_temp_4;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  _INT32 __mplocal_i;
  _INT32 __ompv_temp_do_upper;
  _INT32 __ompv_temp_do_lower;
  _INT32 __ompv_temp_do_stride;
  _INT32 __ompv_temp_last_iter;
  
  __slink_sym_temp_4 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < innerreps)
  {
    temp_limit = nthreads + -1;
    __ompv_temp_do_upper = temp_limit;
    __ompv_temp_do_lower = 0;
    __ompv_temp_last_iter = 0;
    __ompc_static_init_4(__ompv_temp_gtid, 2, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride, 1, 1);
    if(__ompv_temp_do_upper > temp_limit)
    {
      __ompv_temp_do_upper = temp_limit;
    }
    for(__mplocal_i = __ompv_temp_do_lower; __mplocal_i <= __ompv_temp_do_upper; __mplocal_i = __mplocal_i + 1)
    {
      delay(delaylength);
      _1 :;
    }
    __ompc_barrier();
    _2 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testfor1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing FOR time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testfor1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        for(i = 0; i < nthreads; i = i + 1)
        {
          delay(delaylength);
          _3 :;
        }
        _2 :;
        j = j + 1;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("FOR time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("FOR overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testfor */




extern void testpfor()
{
  register _IEEE64 __comma;
  register _INT32 trip_count;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 i;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompdo_testpfor1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 temp_limit;
  _UINT64 __slink_sym_temp_6;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_i;
  _INT32 __ompv_temp_do_upper;
  _INT32 __ompv_temp_do_lower;
  _INT32 __ompv_temp_do_stride;
  _INT32 __ompv_temp_last_iter;
  
  __slink_sym_temp_6 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  temp_limit = nthreads + -1;
  __ompv_temp_do_upper = temp_limit;
  __ompv_temp_do_lower = 0;
  __ompv_temp_last_iter = 0;
  __ompc_static_init_4(__ompv_temp_gtid, 2, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride, 1, 1);
  if(__ompv_temp_do_upper > temp_limit)
  {
    __ompv_temp_do_upper = temp_limit;
  }
  for(__mplocal_i = __ompv_temp_do_lower; __mplocal_i <= __ompv_temp_do_upper; __mplocal_i = __mplocal_i + 1)
  {
    delay(delaylength);
    _1 :;
  }
  return;
} /* __ompdo_testpfor1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing PARALLEL FOR time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      trip_count = nthreads;
      __ompv_ok_to_fork = __ompc_can_fork();
      if((trip_count > 1) && (__ompv_ok_to_fork == 1))
      {
        __ompc_fork(0, &__ompdo_testpfor1, reg7);
      }
      else
      {
        __ompv_gtid_s1 = __ompc_get_local_thread_num();
        __ompc_serialized_parallel();
        for(i = 0; i < nthreads; i = i + 1)
        {
          delay(delaylength);
          _3 :;
        }
        __ompc_end_serialized_parallel();
      }
      _2 :;
      j = j + 1;
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("PARALLEL FOR time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("PARALLEL FOR overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testpfor */




extern void testbar()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testbar1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  _UINT64 __slink_sym_temp_8;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  
  __slink_sym_temp_8 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < innerreps)
  {
    delay(delaylength);
    __ompc_barrier();
    _1 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testbar1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing BARRIER time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testbar1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        delay(delaylength);
        _2 :;
        j = j + 1;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("BARRIER time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("BARRIER overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testbar */




extern void testsing()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testsing1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 mpsp_status;
  _UINT64 __slink_sym_temp_10;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  
  __slink_sym_temp_10 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < innerreps)
  {
    mpsp_status = __ompc_single(__ompv_temp_gtid);
    if(mpsp_status == 1)
    {
      delay(delaylength);
    }
    __ompc_end_single(__ompv_temp_gtid);
    __ompc_barrier();
    _1 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testsing1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing SINGLE time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testsing1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        delay(delaylength);
        _2 :;
        j = j + 1;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("SINGLE time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("SINGLE overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testsing */




extern void testcrit()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testcrit1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  _UINT64 __slink_sym_temp_12;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  
  __slink_sym_temp_12 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < (innerreps / nthreads))
  {
    __ompc_critical(__ompv_temp_gtid, (_UINT64) & __mplock_0);
    delay(delaylength);
    __ompc_end_critical(__ompv_temp_gtid, (_UINT64) & __mplock_0);
    _1 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testcrit1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing CRITICAL time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testcrit1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < (innerreps / nthreads))
      {
        __ompc_critical(__ompv_gtid_s1, (_UINT64) & __mplock_0);
        delay(delaylength);
        __ompc_end_critical(__ompv_gtid_s1, (_UINT64) & __mplock_0);
        _2 :;
        j = j + 1;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("CRITICAL time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("CRITICAL overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testcrit */




extern void testlock()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  struct anonymous21 lock;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testlock1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  _UINT64 __slink_sym_temp_14;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  
  __slink_sym_temp_14 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < (innerreps / nthreads))
  {
    omp_set_lock(&lock);
    delay(delaylength);
    omp_unset_lock(&lock);
    _1 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testlock1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing LOCK/UNLOCK time\n");
  omp_init_lock(&lock);
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testlock1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < (innerreps / nthreads))
      {
        omp_set_lock(&lock);
        delay(delaylength);
        omp_unset_lock(&lock);
        _2 :;
        j = j + 1;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("LOCK/UNLOCK time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("LOCK/UNLOCK overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testlock */




extern void testorder()
{
  register _IEEE64 __comma;
  register _INT32 trip_count;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompdo_testorder1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 temp_limit;
  register _INT32 mpni_status;
  _UINT64 __slink_sym_temp_16;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  _INT32 __ompv_temp_do_upper;
  _INT32 __ompv_temp_do_lower;
  _INT32 __ompv_temp_do_stride;
  _INT32 __ompv_temp_last_iter;
  
  __slink_sym_temp_16 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  temp_limit = innerreps + -1;
  __ompv_temp_do_upper = temp_limit;
  __ompv_temp_do_lower = 0;
  __ompv_temp_last_iter = 0;
  __ompc_scheduler_init_4(__ompv_temp_gtid, 33, __ompv_temp_do_lower, __ompv_temp_do_upper, 1, 1);
  __mplocal_j = __ompv_temp_do_lower;
  mpni_status = __ompc_schedule_next_4(__ompv_temp_gtid, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride);
  while(mpni_status)
  {
    if(__ompv_temp_do_upper > temp_limit)
    {
      __ompv_temp_do_upper = temp_limit;
    }
    for(__mplocal_j = __ompv_temp_do_lower; __mplocal_j <= __ompv_temp_do_upper; __mplocal_j = __mplocal_j + __ompv_temp_do_stride)
    {
      __ompc_ordered(__ompv_temp_gtid);
      delay(delaylength);
      __ompc_end_ordered(__ompv_temp_gtid);
      _1 :;
    }
    mpni_status = __ompc_schedule_next_4(__ompv_temp_gtid, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride);
  }
  return;
} /* __ompdo_testorder1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing ORDERED time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    trip_count = innerreps;
    __ompv_ok_to_fork = __ompc_can_fork();
    if((trip_count > 1) && (__ompv_ok_to_fork == 1))
    {
      __ompc_fork(0, &__ompdo_testorder1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      for(j = 0; j < innerreps; j = j + 1)
      {
        delay(delaylength);
        _2 :;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("ORDERED time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("ORDERED overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testorder */




extern void testatom()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _IEEE32 aaaa;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testatom1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  _UINT64 __slink_sym_temp_18;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  
  __slink_sym_temp_18 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < (innerreps / nthreads))
  {
    __ompc_critical(__ompv_temp_gtid, (_UINT64) & __namelock___OMP_CRITICAL_ATOMIC_F4);
    aaaa = aaaa + 1.0F;
    __ompc_end_critical(__ompv_temp_gtid, (_UINT64) & __namelock___OMP_CRITICAL_ATOMIC_F4);
    _1 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testatom1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing ATOMIC time\n");
  k = 0;
  while(k <= 20)
  {
    aaaa = 0.0F;
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testatom1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < (innerreps / nthreads))
      {
        __ompc_critical(__ompv_gtid_s1, (_UINT64) & __namelock___OMP_CRITICAL_ATOMIC_F4);
        aaaa = aaaa + 1.0F;
        __ompc_end_critical(__ompv_gtid_s1, (_UINT64) & __namelock___OMP_CRITICAL_ATOMIC_F4);
        _2 :;
        j = j + 1;
      }
      __ompc_end_serialized_parallel();
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    if((_IEEE64)(aaaa) < 0.0)
    {
      printf("%f\n", (_IEEE64)(aaaa));
    }
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("ATOMIC time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("ATOMIC overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testatom */




extern void testred()
{
  register _IEEE64 __comma;
  register _INT32 __ompv_ok_to_fork;
  register _UINT64 reg7;
  register _IEEE64 __comma0;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  _INT32 aaaa;
  _INT32 __ompv_gtid_s1;
  
  /*Begin_of_nested_PU(s)*/
static void __ompregion_testred1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  _UINT64 __slink_sym_temp_20;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_aaaa;
  
  __slink_sym_temp_20 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_aaaa = 0;
  delay(delaylength);
  __mplocal_aaaa = __mplocal_aaaa + 1;
  __ompc_critical(__ompv_temp_gtid, (_UINT64) & __mplock_1);
  aaaa = __mplocal_aaaa + aaaa;
  __ompc_end_critical(__ompv_temp_gtid, (_UINT64) & __mplock_1);
  __ompc_barrier();
  return;
} /* __ompregion_testred1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing REDUCTION time\n");
  k = 0;
  while(k <= 20)
  {
    aaaa = 0;
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      __ompv_ok_to_fork = __ompc_can_fork();
      if(__ompv_ok_to_fork == 1)
      {
        __ompc_fork(0, &__ompregion_testred1, reg7);
      }
      else
      {
        __ompv_gtid_s1 = __ompc_get_local_thread_num();
        __ompc_serialized_parallel();
        delay(delaylength);
        aaaa = aaaa + 1;
        __ompc_end_serialized_parallel();
      }
      _2 :;
      j = j + 1;
    }
    __comma0 = getclock();
    times[k] = ((__comma0 - start) * 1.0e+06) / (_IEEE64)(innerreps);
    if(aaaa < 0)
    {
      printf("%d\n", aaaa);
    }
    _1 :;
    k = k + 1;
  }
  stats(&meantime, &sd);
  printf("REDUCTION time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("REDUCTION overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testred */




extern void stats(
  _IEEE64 * mtp,
  _IEEE64 * sdp)
{
  register _IEEE64 __cselect;
  register _IEEE64 __cselect0;
  register _IEEE64 __comma;
  _IEEE64 meantime;
  _IEEE64 totaltime;
  _IEEE64 sumsq;
  _IEEE64 mintime;
  _IEEE64 maxtime;
  _IEEE64 sd;
  _IEEE64 cutoff;
  _INT32 i;
  _INT32 nr;
  
  mintime = 1.0e+10;
  maxtime = 0.0;
  totaltime = 0.0;
  i = 1;
  while(i <= 20)
  {
    if(times[i] > mintime)
    {
      __cselect = mintime;
    }
    else
    {
      __cselect = times[i];
    }
    mintime = __cselect;
    if(times[i] < maxtime)
    {
      __cselect0 = maxtime;
    }
    else
    {
      __cselect0 = times[i];
    }
    maxtime = __cselect0;
    totaltime = times[i] + totaltime;
    _1 :;
    i = i + 1;
  }
  meantime = totaltime / 2.0e+01;
  sumsq = 0.0;
  i = 1;
  while(i <= 20)
  {
    sumsq = sumsq + ((times[i] - meantime) * (times[i] - meantime));
    _2 :;
    i = i + 1;
  }
  __comma = sqrt(sumsq / 1.9e+01);
  sd = __comma;
  cutoff = sd * 3.0;
  nr = 0;
  i = 1;
  while(i <= 20)
  {
    if(cutoff < _F8ABS(times[i] - meantime))
    {
      nr = nr + 1;
    }
    _3 :;
    i = i + 1;
  }
  printf("\n");
  printf("Sample_size       Average     Min         Max          S.D.          Outliers\n");
  printf(" %d                %f   %f   %f    %f      %d\n", 20, meantime, mintime, maxtime, sd, nr);
  printf("\n");
  * mtp = meantime;
  * sdp = sd;
  return;
} /* stats */

