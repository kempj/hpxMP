/*******************************************************
 * C file translated from WHIRL Wed Nov  9 19:49:26 2005
 *******************************************************/

/* Include file-level type and variable decls */
#include "schedbench.w2c.h"


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
  printf("Running OpenMP benchmark on %d thread(s)\n", nthreads);
  getdelay();
  itersperthr = 1024;
  innerreps = 1000;
  refer();
  teststatic();
  cksz = 1;
  while(itersperthr >= cksz)
  {
    teststaticn();
    cksz = cksz * 2;
  }
  cksz = 1;
  while(itersperthr >= cksz)
  {
    testdynamicn();
    cksz = cksz * 2;
  }
  cksz = 1;
  while(cksz <= ((itersperthr * 2) / nthreads))
  {
    testguidedn();
    cksz = cksz * 2;
  }
  return reg8;
} /* main */




extern void getdelay()
{
  register _IEEE64 __comma;
  register _IEEE64 __comma0;
  register _IEEE64 __comma1;
  register _IEEE64 __comma2;
  _INT32 i;
  _INT32 reps;
  _IEEE64 actualtime;
  _IEEE64 targettime;
  _IEEE64 start;
  
  delaylength = 0;
  reps = 10000;
  actualtime = 0.0;
  targettime = 6.6666666666666668e-08;
  delay(delaylength);
  while(actualtime < targettime)
  {
    delaylength = _I4F8TRUNC(((_IEEE64)(delaylength) * 1.1000000000000001) + 1.0);
    __comma = getclock();
    start = __comma;
    i = 0;
    while(i < reps)
    {
      delay(delaylength);
      _1 :;
      i = i + 1;
    }
    __comma0 = getclock();
    actualtime = (__comma0 - start) / (_IEEE64)(reps);
  }
  __comma1 = getclock();
  start = __comma1;
  i = 0;
  while(i < reps)
  {
    delay(delaylength);
    _2 :;
    i = i + 1;
  }
  __comma2 = getclock();
  actualtime = (__comma2 - start) / (_IEEE64)(reps);
  printf("Assumed clock rate = %d MHz \n", 1500);
  printf("Delay length = %d\n", delaylength);
  printf("Delay time  = %f cycles\n", (actualtime * 1.5e+03) * 1.0e+06);
  return;
} /* getdelay */


extern void refer()
{
  register _IEEE64 __comma;
  register _IEEE64 __comma0;
  _INT32 i;
  _INT32 j;
  _INT32 k;
  _IEEE64 start;
  _IEEE64 meantime;
  _IEEE64 sd;
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing reference time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    j = 0;
    while(j < innerreps)
    {
      i = 0;
      while(i < itersperthr)
      {
        delay(delaylength);
        _3 :;
        i = i + 1;
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
  printf("Reference_time_1 =                        %f microseconds +/- %f\n", meantime, sd * 1.96);
  reftime = meantime;
  refsd = sd;
  return;
} /* refer */


extern void teststatic()
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
static void __ompregion_teststatic1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 temp_limit;
  _UINT64 __slink_sym_temp_2;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  _INT32 __mplocal_i;
  _INT32 __ompv_temp_do_upper;
  _INT32 __ompv_temp_do_lower;
  _INT32 __ompv_temp_do_stride;
  _INT32 __ompv_temp_last_iter;
  
  __slink_sym_temp_2 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < innerreps)
  {
    temp_limit = (nthreads * itersperthr) + -1;
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
} /* __ompregion_teststatic1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing STATIC time\n");
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_teststatic1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        for(i = 0; i < (nthreads * itersperthr); i = i + 1)
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
  printf("STATIC time =                           %f microseconds +/- %f\n", meantime, sd * 1.96);
  printf("STATIC overhead =                       %f microseconds +/- %f\n", meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* teststatic */




extern void teststaticn()
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
static void __ompregion_teststaticn1(__ompv_gtid_a, __ompv_slink_a)
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
    temp_limit = (nthreads * itersperthr) + -1;
    __ompv_temp_do_upper = temp_limit;
    __ompv_temp_do_lower = 0;
    __ompv_temp_last_iter = 0;
    __ompc_static_init_4(__ompv_temp_gtid, 1, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride, 1, cksz);
    while(__ompv_temp_do_lower <= temp_limit)
    {
      if(__ompv_temp_do_upper > temp_limit)
      {
        __ompv_temp_do_upper = temp_limit;
      }
      for(__mplocal_i = __ompv_temp_do_lower; __mplocal_i <= __ompv_temp_do_upper; __mplocal_i = __mplocal_i + 1)
      {
        delay(delaylength);
        _1 :;
      }
      __ompv_temp_do_lower = __ompv_temp_do_lower + __ompv_temp_do_stride;
      __ompv_temp_do_upper = __ompv_temp_do_upper + __ompv_temp_do_stride;
    }
    __ompc_barrier();
    _2 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_teststaticn1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing STATIC %d time\n", cksz);
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_teststaticn1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        for(i = 0; i < (nthreads * itersperthr); i = i + 1)
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
  printf("STATIC %d time =                           %f microseconds +/- %f\n", cksz, meantime, sd * 1.96);
  printf("STATIC %d overhead =                       %f microseconds +/- %f\n", cksz, meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* teststaticn */




extern void testdynamicn()
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
static void __ompregion_testdynamicn1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 temp_limit;
  register _INT32 mpni_status;
  _UINT64 __slink_sym_temp_6;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  _INT32 __mplocal_i;
  _INT32 __ompv_temp_do_upper;
  _INT32 __ompv_temp_do_lower;
  _INT32 __ompv_temp_do_stride;
  _INT32 __ompv_temp_last_iter;
  
  __slink_sym_temp_6 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < innerreps)
  {
    temp_limit = (nthreads * itersperthr) + -1;
    __ompv_temp_do_upper = temp_limit;
    __ompv_temp_do_lower = 0;
    __ompv_temp_last_iter = 0;
    __ompc_scheduler_init_4(__ompv_temp_gtid, 3, __ompv_temp_do_lower, __ompv_temp_do_upper, 1, cksz);
    __mplocal_i = __ompv_temp_do_lower;
    mpni_status = __ompc_schedule_next_4(__ompv_temp_gtid, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride);
    while(mpni_status)
    {
      if(__ompv_temp_do_upper > temp_limit)
      {
        __ompv_temp_do_upper = temp_limit;
      }
      for(__mplocal_i = __ompv_temp_do_lower; __mplocal_i <= __ompv_temp_do_upper; __mplocal_i = __mplocal_i + __ompv_temp_do_stride)
      {
        delay(delaylength);
        _1 :;
      }
      mpni_status = __ompc_schedule_next_4(__ompv_temp_gtid, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride);
    }
    __ompc_barrier();
    _2 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testdynamicn1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing DYNAMIC %d time\n", cksz);
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testdynamicn1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        for(i = 0; i < (nthreads * itersperthr); i = i + 1)
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
  printf("DYNAMIC %d time =                           %f microseconds +/- %f\n", cksz, meantime, sd * 1.96);
  printf("DYNAMIC %d overhead =                       %f microseconds +/- %f\n", cksz, meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testdynamicn */




extern void testguidedn()
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
static void __ompregion_testguidedn1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  register _INT32 temp_limit;
  register _INT32 mpni_status;
  _UINT64 __slink_sym_temp_8;
  _INT32 __ompv_temp_gtid;
  _INT32 __mplocal_j;
  _INT32 __mplocal_i;
  _INT32 __ompv_temp_do_upper;
  _INT32 __ompv_temp_do_lower;
  _INT32 __ompv_temp_do_stride;
  _INT32 __ompv_temp_last_iter;
  
  __slink_sym_temp_8 = __ompv_slink_a;
  __ompv_temp_gtid = __ompv_gtid_a;
  __mplocal_j = 0;
  while(__mplocal_j < innerreps)
  {
    temp_limit = (nthreads * itersperthr) + -1;
    __ompv_temp_do_upper = temp_limit;
    __ompv_temp_do_lower = 0;
    __ompv_temp_last_iter = 0;
    __ompc_scheduler_init_4(__ompv_temp_gtid, 4, __ompv_temp_do_lower, __ompv_temp_do_upper, 1, cksz);
    __mplocal_i = __ompv_temp_do_lower;
    mpni_status = __ompc_schedule_next_4(__ompv_temp_gtid, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride);
    while(mpni_status)
    {
      if(__ompv_temp_do_upper > temp_limit)
      {
        __ompv_temp_do_upper = temp_limit;
      }
      for(__mplocal_i = __ompv_temp_do_lower; __mplocal_i <= __ompv_temp_do_upper; __mplocal_i = __mplocal_i + __ompv_temp_do_stride)
      {
        delay(delaylength);
        _1 :;
      }
      mpni_status = __ompc_schedule_next_4(__ompv_temp_gtid, &__ompv_temp_do_lower, &__ompv_temp_do_upper, &__ompv_temp_do_stride);
    }
    __ompc_barrier();
    _2 :;
    __mplocal_j = __mplocal_j + 1;
  }
  return;
} /* __ompregion_testguidedn1 */
  
  printf("\n");
  printf("--------------------------------------------------------\n");
  printf("Computing GUIDED %d time\n", cksz);
  k = 0;
  while(k <= 20)
  {
    __comma = getclock();
    start = __comma;
    __ompv_ok_to_fork = __ompc_can_fork();
    if(__ompv_ok_to_fork == 1)
    {
      __ompc_fork(0, &__ompregion_testguidedn1, reg7);
    }
    else
    {
      __ompv_gtid_s1 = __ompc_get_local_thread_num();
      __ompc_serialized_parallel();
      j = 0;
      while(j < innerreps)
      {
        for(i = 0; i < (nthreads * itersperthr); i = i + 1)
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
  printf("GUIDED %d time =                           %f microseconds +/- %f\n", cksz, meantime, sd * 1.96);
  printf("GUIDED %d overhead =                       %f microseconds +/- %f\n", cksz, meantime - reftime, (sd + refsd) * 1.96);
  return;
} /* testguidedn */




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

