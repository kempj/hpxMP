/*******************************************************
 * C file translated from WHIRL Tue Jun  3 12:11:52 2014
 *******************************************************/

/* Include file-level type and variable decls */
#include "omp-for-dynamic.w2c.h"


__inline _INT32 vprintf(
  const _INT8 * restrict __fmt,
  struct __va_list_tag * __arg)
{
  
  register _UINT64 _w2c___fmt0;
  register _UINT64 _w2c_reg5;
  register _UINT64 _w2c___arg0;
  register _UINT64 _w2c_reg6;
  register _INT64 _w2c_reg7;
  register _INT32 _w2c___comma;
  
  _w2c___fmt0 = _w2c_reg5;
  _w2c___arg0 = _w2c_reg6;
  _w2c_reg5 = (_UINT64)(stdout);
  _w2c_reg6 = (_UINT64)((const _INT8 *) _w2c___fmt0);
  _w2c_reg7 = (_INT64)((struct __va_list_tag *) _w2c___arg0);
  _w2c___comma = vfprintf(stdout, (const _INT8 *) _w2c___fmt0, (struct __va_list_tag *) _w2c___arg0);
  return _w2c___comma;
} /* vprintf */


__inline _INT32 getchar()
{
  
  register _INT64 _w2c_reg5;
  register _INT32 _w2c___comma;
  
  _w2c_reg5 = (_INT64)(stdin);
  _w2c___comma = _IO_getc(stdin);
  return _w2c___comma;
} /* getchar */


__inline _INT32 fgetc_unlocked(
  struct _IO_FILE * __fp)
{
  
  register _UINT64 _w2c___fp0;
  register _UINT64 _w2c_reg5;
  register _UINT64 _w2c_preg;
  struct _IO_FILE _tmp0;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___cselect;
  
  _w2c___fp0 = _w2c_reg5;
  _tmp0 = (struct _IO_FILE)((struct _IO_FILE *) _w2c___fp0) -> _IO_read_ptr;
  _w2c_preg = *(_UINT64 *) & _tmp0;
  if(!((_INT64)((_INT64) EXPECT((_UINT64)((struct _IO_FILE)((struct _IO_FILE *) _w2c___fp0) -> _IO_read_end) <= _w2c_preg, (_INT64) 0U)) != (_INT64)(0U)))
    goto _770;
  _w2c_reg5 = (_UINT64)((struct _IO_FILE *) _w2c___fp0);
  _w2c___comma = __uflow((struct _IO_FILE *) _w2c___fp0);
  _w2c___cselect = _w2c___comma;
  goto _1026;
  _770 :;
  ((struct _IO_FILE *) _w2c___fp0) -> _IO_read_ptr = (_INT8 *)(_w2c_preg + (_UINT64)(1U));
  _w2c___cselect = (_INT32) * ((_UINT8 *) _w2c_preg);
  _1026 :;
  return _w2c___cselect;
} /* fgetc_unlocked */


__inline _INT32 getc_unlocked(
  struct _IO_FILE * __fp)
{
  
  register _UINT64 _w2c___fp0;
  register _UINT64 _w2c_reg5;
  register _UINT64 _w2c_preg;
  struct _IO_FILE _tmp0;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___cselect;
  
  _w2c___fp0 = _w2c_reg5;
  _tmp0 = (struct _IO_FILE)((struct _IO_FILE *) _w2c___fp0) -> _IO_read_ptr;
  _w2c_preg = *(_UINT64 *) & _tmp0;
  if(!((_INT64)((_INT64) EXPECT((_UINT64)((struct _IO_FILE)((struct _IO_FILE *) _w2c___fp0) -> _IO_read_end) <= _w2c_preg, (_INT64) 0U)) != (_INT64)(0U)))
    goto _770;
  _w2c_reg5 = (_UINT64)((struct _IO_FILE *) _w2c___fp0);
  _w2c___comma = __uflow((struct _IO_FILE *) _w2c___fp0);
  _w2c___cselect = _w2c___comma;
  goto _1026;
  _770 :;
  ((struct _IO_FILE *) _w2c___fp0) -> _IO_read_ptr = (_INT8 *)(_w2c_preg + (_UINT64)(1U));
  _w2c___cselect = (_INT32) * ((_UINT8 *) _w2c_preg);
  _1026 :;
  return _w2c___cselect;
} /* getc_unlocked */


__inline _INT32 getchar_unlocked()
{
  
  register _UINT64 _w2c_stdin0;
  register _UINT64 _w2c_preg;
  struct _IO_FILE _tmp0;
  register _INT64 _w2c_reg5;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___cselect;
  
  _w2c_stdin0 = (_UINT64)(stdin);
  _tmp0 = (struct _IO_FILE)((struct _IO_FILE *) _w2c_stdin0) -> _IO_read_ptr;
  _w2c_preg = *(_UINT64 *) & _tmp0;
  if(!((_INT64)((_INT64) EXPECT((_UINT64)((struct _IO_FILE)((struct _IO_FILE *) _w2c_stdin0) -> _IO_read_end) <= _w2c_preg, (_INT64) 0U)) != (_INT64)(0U)))
    goto _770;
  _w2c_reg5 = (_INT64)((struct _IO_FILE *) _w2c_stdin0);
  _w2c___comma = __uflow((struct _IO_FILE *) _w2c_stdin0);
  _w2c___cselect = _w2c___comma;
  goto _1026;
  _770 :;
  ((struct _IO_FILE *) _w2c_stdin0) -> _IO_read_ptr = (_INT8 *)(_w2c_preg + (_UINT64)(1U));
  _w2c___cselect = (_INT32) * ((_UINT8 *) _w2c_preg);
  _1026 :;
  return _w2c___cselect;
} /* getchar_unlocked */


__inline _INT32 putchar(
  _INT32 __c)
{
  
  register _INT32 _w2c___c0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg6;
  register _INT32 _w2c___comma;
  
  _w2c___c0 = (_INT32) _w2c_reg5;
  _w2c_reg5 = _w2c___c0;
  _w2c_reg6 = (_INT64)(stdout);
  _w2c___comma = _IO_putc(_w2c___c0, stdout);
  return _w2c___comma;
} /* putchar */


__inline _INT32 fputc_unlocked(
  _INT32 __c,
  struct _IO_FILE * __stream)
{
  
  register _INT32 _w2c___c0;
  register _INT64 _w2c_reg5;
  register _UINT64 _w2c___stream0;
  register _UINT64 _w2c_reg6;
  register _UINT64 _w2c_preg;
  struct _IO_FILE _tmp0;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___cselect;
  
  _w2c___c0 = (_INT32) _w2c_reg5;
  _w2c___stream0 = _w2c_reg6;
  _tmp0 = (struct _IO_FILE)((struct _IO_FILE *) _w2c___stream0) -> _IO_write_ptr;
  _w2c_preg = *(_UINT64 *) & _tmp0;
  if(!((_INT64)((_INT64) EXPECT((_UINT64)((struct _IO_FILE)((struct _IO_FILE *) _w2c___stream0) -> _IO_write_end) <= _w2c_preg, (_INT64) 0U)) != (_INT64)(0U)))
    goto _770;
  _w2c_reg5 = (_INT64)((struct _IO_FILE *) _w2c___stream0);
  _w2c_reg6 = (_UINT8)(_w2c___c0);
  _w2c___comma = __overflow((struct _IO_FILE *) _w2c___stream0, (_INT32)(_UINT8)(_w2c___c0));
  _w2c___cselect = _w2c___comma;
  goto _1026;
  _770 :;
  ((struct _IO_FILE *) _w2c___stream0) -> _IO_write_ptr = (_INT8 *)(_w2c_preg + (_UINT64)(1U));
  * ((_INT8 *) _w2c_preg) = _w2c___c0;
  _w2c___cselect = (_UINT8)(_w2c___c0);
  _1026 :;
  return _w2c___cselect;
} /* fputc_unlocked */


__inline _INT32 putc_unlocked(
  _INT32 __c,
  struct _IO_FILE * __stream)
{
  
  register _INT32 _w2c___c0;
  register _INT64 _w2c_reg5;
  register _UINT64 _w2c___stream0;
  register _UINT64 _w2c_reg6;
  register _UINT64 _w2c_preg;
  struct _IO_FILE _tmp0;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___cselect;
  
  _w2c___c0 = (_INT32) _w2c_reg5;
  _w2c___stream0 = _w2c_reg6;
  _tmp0 = (struct _IO_FILE)((struct _IO_FILE *) _w2c___stream0) -> _IO_write_ptr;
  _w2c_preg = *(_UINT64 *) & _tmp0;
  if(!((_INT64)((_INT64) EXPECT((_UINT64)((struct _IO_FILE)((struct _IO_FILE *) _w2c___stream0) -> _IO_write_end) <= _w2c_preg, (_INT64) 0U)) != (_INT64)(0U)))
    goto _770;
  _w2c_reg5 = (_INT64)((struct _IO_FILE *) _w2c___stream0);
  _w2c_reg6 = (_UINT8)(_w2c___c0);
  _w2c___comma = __overflow((struct _IO_FILE *) _w2c___stream0, (_INT32)(_UINT8)(_w2c___c0));
  _w2c___cselect = _w2c___comma;
  goto _1026;
  _770 :;
  ((struct _IO_FILE *) _w2c___stream0) -> _IO_write_ptr = (_INT8 *)(_w2c_preg + (_UINT64)(1U));
  * ((_INT8 *) _w2c_preg) = _w2c___c0;
  _w2c___cselect = (_UINT8)(_w2c___c0);
  _1026 :;
  return _w2c___cselect;
} /* putc_unlocked */


__inline _INT32 putchar_unlocked(
  _INT32 __c)
{
  
  register _INT32 _w2c___c0;
  register _INT64 _w2c_reg5;
  register _UINT64 _w2c_stdout0;
  register _UINT64 _w2c_preg;
  struct _IO_FILE _tmp0;
  register _INT64 _w2c_reg6;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___cselect;
  
  _w2c___c0 = (_INT32) _w2c_reg5;
  _w2c_stdout0 = (_UINT64)(stdout);
  _tmp0 = (struct _IO_FILE)((struct _IO_FILE *) _w2c_stdout0) -> _IO_write_ptr;
  _w2c_preg = *(_UINT64 *) & _tmp0;
  if(!((_INT64)((_INT64) EXPECT((_UINT64)((struct _IO_FILE)((struct _IO_FILE *) _w2c_stdout0) -> _IO_write_end) <= _w2c_preg, (_INT64) 0U)) != (_INT64)(0U)))
    goto _770;
  _w2c_reg5 = (_INT64)((struct _IO_FILE *) _w2c_stdout0);
  _w2c_reg6 = (_UINT8)(_w2c___c0);
  _w2c___comma = __overflow((struct _IO_FILE *) _w2c_stdout0, (_INT32)(_UINT8)(_w2c___c0));
  _w2c___cselect = _w2c___comma;
  goto _1026;
  _770 :;
  ((struct _IO_FILE *) _w2c_stdout0) -> _IO_write_ptr = (_INT8 *)(_w2c_preg + (_UINT64)(1U));
  * ((_INT8 *) _w2c_preg) = _w2c___c0;
  _w2c___cselect = (_UINT8)(_w2c___c0);
  _1026 :;
  return _w2c___cselect;
} /* putchar_unlocked */


__inline _INT32 feof_unlocked(
  struct _IO_FILE * __stream)
{
  
  register _UINT64 _w2c___stream0;
  register _UINT64 _w2c_reg5;
  
  _w2c___stream0 = _w2c_reg5;
  return((_INT32)((struct _IO_FILE)((struct _IO_FILE *) _w2c___stream0) -> _flags) & (_INT32)(16U)) != (_INT32)(0U);
} /* feof_unlocked */


__inline _INT32 ferror_unlocked(
  struct _IO_FILE * __stream)
{
  
  register _UINT64 _w2c___stream0;
  register _UINT64 _w2c_reg5;
  
  _w2c___stream0 = _w2c_reg5;
  return((_INT32)((struct _IO_FILE)((struct _IO_FILE *) _w2c___stream0) -> _flags) & (_INT32)(32U)) != (_INT32)(0U);
} /* ferror_unlocked */


__inline _INT32 atoi(
  const _INT8 * __nptr)
{
  
  register _UINT64 _w2c___nptr0;
  register _UINT64 _w2c_reg5;
  register _INT64 _w2c_reg6;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c___comma;
  
  _w2c___nptr0 = _w2c_reg5;
  _w2c_reg5 = (_UINT64)((const _INT8 *) _w2c___nptr0);
  _w2c_reg6 = 0U;
  _w2c_reg7 = 10U;
  _w2c___comma = strtol((const _INT8 *) _w2c___nptr0, (_INT8 **) 0U, (_INT32) 10U);
  return(_INT32)(_w2c___comma);
} /* atoi */


__inline _INT64 atol(
  const _INT8 * __nptr)
{
  
  register _UINT64 _w2c___nptr0;
  register _UINT64 _w2c_reg5;
  register _INT64 _w2c_reg6;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c___comma;
  
  _w2c___nptr0 = _w2c_reg5;
  _w2c_reg5 = (_UINT64)((const _INT8 *) _w2c___nptr0);
  _w2c_reg6 = 0U;
  _w2c_reg7 = 10U;
  _w2c___comma = strtol((const _INT8 *) _w2c___nptr0, (_INT8 **) 0U, (_INT32) 10U);
  return _w2c___comma;
} /* atol */


__inline _INT64 atoll(
  const _INT8 * __nptr)
{
  
  register _UINT64 _w2c___nptr0;
  register _UINT64 _w2c_reg5;
  register _INT64 _w2c_reg6;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c___comma;
  
  _w2c___nptr0 = _w2c_reg5;
  _w2c_reg5 = (_UINT64)((const _INT8 *) _w2c___nptr0);
  _w2c_reg6 = 0U;
  _w2c_reg7 = 10U;
  _w2c___comma = strtoll((const _INT8 *) _w2c___nptr0, (_INT8 **) 0U, (_INT32) 10U);
  return _w2c___comma;
} /* atoll */


__inline _UINT32 gnu_dev_major(
  _UINT64 __dev)
{
  
  register _UINT64 _w2c___dev0;
  register _UINT64 _w2c_reg5;
  
  _w2c___dev0 = _w2c_reg5;
  return((_UINT32)(_U8LSHR(_w2c___dev0, (_UINT64)(8U))) & 4095U) | ((_UINT32)(_U8LSHR(_w2c___dev0, (_UINT64)(32U))) & 4294963200U);
} /* gnu_dev_major */


__inline _UINT32 gnu_dev_minor(
  _UINT64 __dev)
{
  
  register _UINT64 _w2c___dev0;
  register _UINT64 _w2c_reg5;
  
  _w2c___dev0 = _w2c_reg5;
  return((_UINT32)(_w2c___dev0) & 255U) | ((_UINT32)(_U8LSHR(_w2c___dev0, (_UINT64)(12U))) & 4294967040U);
} /* gnu_dev_minor */


__inline _UINT64 gnu_dev_makedev(
  _UINT32 __major,
  _UINT32 __minor)
{
  
  register _UINT32 _w2c___major0;
  register _UINT32 _w2c_reg5;
  register _UINT32 _w2c___minor0;
  register _UINT32 _w2c_reg6;
  
  _w2c___major0 = _w2c_reg5;
  _w2c___minor0 = _w2c_reg6;
  return((_UINT64)((_w2c___minor0 & 255U) | _U4SHL(_w2c___major0 & 4095U, 8U)) | _U8SHL((_UINT64)(_w2c___minor0) & (_UINT64)(4294967040U), (_UINT64)(12U))) | _U8SHL((_UINT64)(_w2c___major0) & (_UINT64)(4294963200U), (_UINT64)(32U));
} /* gnu_dev_makedev */


__inline void * bsearch(
  const void * __key,
  const void * __base,
  _UINT64 __nmemb,
  _UINT64 __size,
  _INT32(*__compar)(const void *, const void *))
{
  
  register _UINT64 _w2c___key0;
  register _UINT64 _w2c_reg5;
  register _UINT64 _w2c___base0;
  register _UINT64 _w2c_reg6;
  register _UINT64 _w2c___nmemb0;
  register _UINT64 _w2c_reg7;
  register _UINT64 _w2c___size0;
  register _UINT64 _w2c_reg8;
  register _UINT64 _w2c___compar0;
  register _UINT64 _w2c_reg9;
  register _UINT64 _w2c___u0;
  register _UINT64 _w2c___l0;
  register _UINT64 _w2c_preg;
  register _UINT64 _w2c_preg0;
  register _INT32 _w2c___comma;
  
  _w2c___key0 = _w2c_reg5;
  _w2c___base0 = _w2c_reg6;
  _w2c___nmemb0 = _w2c_reg7;
  _w2c___size0 = _w2c_reg8;
  _w2c___compar0 = _w2c_reg9;
  _w2c___u0 = _w2c___nmemb0;
  if(!(_w2c___nmemb0 > (_UINT64)(0U)))
    goto _2562;
  _w2c___l0 = 0U;
  _4354 :;
  _w2c_preg = (_w2c___u0 + _w2c___l0) / (_UINT64)(2U);
  _w2c_preg0 = (_UINT64)((const void *) _w2c___base0) + (_w2c_preg * _w2c___size0);
  _w2c_reg5 = (_UINT64)((const void *) _w2c___key0);
  _w2c_reg6 = _w2c_preg0;
  _w2c___comma = (((_INT32(*)(const void *, const void *)) _w2c___compar0))((const void *) _w2c___key0, (const void *) _w2c_preg0);
  if(!(_w2c___comma < (_INT32)(0U)))
    goto _3330;
  _w2c___u0 = _w2c_preg;
  goto _258;
  _3330 :;
  if(!(_w2c___comma > (_INT32)(0U)))
    goto _3586;
  _w2c___l0 = _w2c_preg + (_UINT64)(1U);
  _258 :;
  if(_w2c___u0 > _w2c___l0)
    goto _4354;
  goto _2562;
  _3586 :;
  return(void *) _w2c_preg0;
  _2562 :;
  return(void *) 0U;
} /* bsearch */


__inline _IEEE64 atof(
  const _INT8 * __nptr)
{
  
  register _UINT64 _w2c___nptr0;
  register _UINT64 _w2c_reg5;
  register _INT64 _w2c_reg6;
  register _IEEE64 _w2c___comma;
  
  _w2c___nptr0 = _w2c_reg5;
  _w2c_reg5 = (_UINT64)((const _INT8 *) _w2c___nptr0);
  _w2c_reg6 = 0U;
  _w2c___comma = strtod((const _INT8 *) _w2c___nptr0, (_INT8 **) 0U);
  return _w2c___comma;
} /* atof */


__inline _INT32 pthread_equal(
  _UINT64 __thread1,
  _UINT64 __thread2)
{
  
  register _UINT64 _w2c___thread;
  register _UINT64 _w2c_reg5;
  register _UINT64 _w2c___thread0;
  register _UINT64 _w2c_reg6;
  
  _w2c___thread = _w2c_reg5;
  _w2c___thread0 = _w2c_reg6;
  return _w2c___thread == _w2c___thread0;
} /* pthread_equal */


extern _INT32 main()
{
  
  register _INT64 _w2c_reg1;
  register _INT32 _w2c___ompv_ok_to_fork;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg6;
  register _INT64 _w2c_reg7;
  register _UINT64 _w2c_reg3;
  register _INT32 _w2c_i0;
  register _INT32 _w2c___ompv_ok_to_fork0;
  register _INT32 _w2c___ompv_ok_to_fork1;
  register _INT32 _w2c___ompv_ok_to_fork2;
  register _INT32 _w2c___comma;
  register _INT32 _w2c___ompv_ok_to_fork3;
  register _INT32 _w2c___comma0;
  register _INT32 _w2c___ompv_ok_to_fork4;
  register _INT32 _w2c___comma1;
  _INT32 i;
  _INT32 _temp__mp_xpragma8;
  _INT32 _temp__mp_xpragma9;
  
  /*Begin_of_nested_PU(s)*/
  
  _w2c_reg1 = 0;
  _w2c___ompv_ok_to_fork = __ompc_can_fork();
  if(!(_w2c___ompv_ok_to_fork))
    goto _514;
  _w2c_reg5 = 0U;
  _w2c_reg6 = (_INT64)(&__ompdo_main_1);
  _w2c_reg7 = _w2c_reg3;
  _w2c_reg1 = 0;
  __ompc_fork((_INT32) 0U, &__ompdo_main_1, _w2c_reg3);
  goto _9474;
  _514 :;
  _w2c_reg1 = 0;
  __ompc_get_local_thread_num();
  _w2c_reg1 = 0;
  __ompc_serialized_parallel();
  _w2c_i0 = 0U;
  _1282 :;
  _w2c_reg5 = (_INT64)("Static Hello World %d\n");
  _w2c_reg6 = _w2c_i0;
  _w2c_reg1 = 0;
  printf("Static Hello World %d\n", _w2c_i0);
  _w2c_i0 = _w2c_i0 + (_INT32)(1U);
  if(_w2c_i0 <= (_INT32)(10U))
    goto _1282;
  _w2c_reg1 = 0;
  __ompc_end_serialized_parallel();
  _9474 :;
  _temp__mp_xpragma8 = 1U;
  _w2c_reg1 = 0;
  _w2c___ompv_ok_to_fork0 = __ompc_can_fork();
  if(!(_w2c___ompv_ok_to_fork0))
    goto _2050;
  _w2c_reg5 = 0U;
  _w2c_reg6 = (_INT64)(&__ompdo_main_2);
  _w2c_reg7 = _w2c_reg3;
  _w2c_reg1 = 0;
  __ompc_fork((_INT32) 0U, &__ompdo_main_2, _w2c_reg3);
  goto _9730;
  _2050 :;
  _w2c_reg1 = 0;
  __ompc_get_local_thread_num();
  _w2c_reg1 = 0;
  __ompc_serialized_parallel();
  _w2c_i0 = 0U;
  _2818 :;
  _w2c_reg5 = (_INT64)("Static1 Hello World %d\n");
  _w2c_reg6 = _w2c_i0;
  _w2c_reg1 = 0;
  printf("Static1 Hello World %d\n", _w2c_i0);
  _w2c_i0 = _w2c_i0 + (_INT32)(1U);
  if(_w2c_i0 <= (_INT32)(10U))
    goto _2818;
  _w2c_reg1 = 0;
  __ompc_end_serialized_parallel();
  _9730 :;
  _temp__mp_xpragma9 = 2U;
  _w2c_reg1 = 0;
  _w2c___ompv_ok_to_fork1 = __ompc_can_fork();
  if(!(_w2c___ompv_ok_to_fork1))
    goto _3586;
  _w2c_reg5 = 0U;
  _w2c_reg6 = (_INT64)(&__ompdo_main_3);
  _w2c_reg7 = _w2c_reg3;
  _w2c_reg1 = 0;
  __ompc_fork((_INT32) 0U, &__ompdo_main_3, _w2c_reg3);
  goto _9986;
  _3586 :;
  _w2c_reg1 = 0;
  __ompc_get_local_thread_num();
  _w2c_reg1 = 0;
  __ompc_serialized_parallel();
  _w2c_i0 = 0U;
  _4354 :;
  _w2c_reg5 = (_INT64)("Static2 Hello World %d\n");
  _w2c_reg6 = _w2c_i0;
  _w2c_reg1 = 0;
  printf("Static2 Hello World %d\n", _w2c_i0);
  _w2c_i0 = _w2c_i0 + (_INT32)(1U);
  if(_w2c_i0 <= (_INT32)(10U))
    goto _4354;
  _w2c_reg1 = 0;
  __ompc_end_serialized_parallel();
  _9986 :;
  _w2c_reg5 = (_INT64)("Dynamic loop\n");
  _w2c_reg1 = 0;
  printf("Dynamic loop\n");
  _w2c_reg1 = 0;
  _w2c___ompv_ok_to_fork2 = __ompc_can_fork();
  if(!(_w2c___ompv_ok_to_fork2))
    goto _5122;
  _w2c_reg5 = 0U;
  _w2c_reg6 = (_INT64)(&__ompdo_main_4);
  _w2c_reg7 = _w2c_reg3;
  _w2c_reg1 = 0;
  __ompc_fork((_INT32) 0U, &__ompdo_main_4, _w2c_reg3);
  goto _10242;
  _5122 :;
  _w2c_reg1 = 0;
  __ompc_get_local_thread_num();
  _w2c_reg1 = 0;
  __ompc_serialized_parallel();
  _w2c_i0 = 0U;
  _5890 :;
  _w2c___comma = omp_get_thread_num();
  _w2c_reg5 = (_INT64)("Thread %d: Dynamic Hello World %d\n");
  _w2c_reg6 = _w2c___comma;
  _w2c_reg7 = _w2c_i0;
  _w2c_reg1 = 0;
  printf("Thread %d: Dynamic Hello World %d\n", _w2c___comma, _w2c_i0);
  _w2c_i0 = _w2c_i0 + (_INT32)(1U);
  if(_w2c_i0 <= (_INT32)(8U))
    goto _5890;
  _w2c_reg1 = 0;
  __ompc_end_serialized_parallel();
  _10242 :;
  _w2c_reg5 = (_INT64)("Static Ordered loop\n");
  _w2c_reg1 = 0;
  printf("Static Ordered loop\n");
  _w2c_reg1 = 0;
  _w2c___ompv_ok_to_fork3 = __ompc_can_fork();
  if(!(_w2c___ompv_ok_to_fork3))
    goto _6658;
  _w2c_reg5 = 0U;
  _w2c_reg6 = (_INT64)(&__ompdo_main_5);
  _w2c_reg7 = _w2c_reg3;
  _w2c_reg1 = 0;
  __ompc_fork((_INT32) 0U, &__ompdo_main_5, _w2c_reg3);
  goto _10498;
  _6658 :;
  _w2c_reg1 = 0;
  __ompc_get_local_thread_num();
  _w2c_reg1 = 0;
  __ompc_serialized_parallel();
  i = 0U;
  _7426 :;
  _w2c___comma0 = omp_get_thread_num();
  _w2c_reg5 = (_INT64)("Thread %d: Static Ordered Hello World %d\n");
  _w2c_reg6 = _w2c___comma0;
  _w2c_reg7 = i;
  _w2c_reg1 = 0;
  printf("Thread %d: Static Ordered Hello World %d\n", _w2c___comma0, i);
  _w2c_i0 = i + (_INT32)(1U);
  i = _w2c_i0;
  if(_w2c_i0 <= (_INT32)(9U))
    goto _7426;
  _w2c_reg1 = 0;
  __ompc_end_serialized_parallel();
  _10498 :;
  _w2c_reg5 = (_INT64)("Dynamic Ordered loop\n");
  _w2c_reg1 = 0;
  printf("Dynamic Ordered loop\n");
  _w2c_reg1 = 0;
  _w2c___ompv_ok_to_fork4 = __ompc_can_fork();
  if(!(_w2c___ompv_ok_to_fork4))
    goto _8194;
  _w2c_reg5 = 0U;
  _w2c_reg6 = (_INT64)(&__ompdo_main_6);
  _w2c_reg7 = _w2c_reg3;
  _w2c_reg1 = 0;
  __ompc_fork((_INT32) 0U, &__ompdo_main_6, _w2c_reg3);
  goto _10754;
  _8194 :;
  _w2c_reg1 = 0;
  __ompc_get_local_thread_num();
  _w2c_reg1 = 0;
  __ompc_serialized_parallel();
  i = 0U;
  _8962 :;
  _w2c___comma1 = omp_get_thread_num();
  _w2c_reg5 = (_INT64)("Thread %d: Dynamic Ordered Hello World %d\n");
  _w2c_reg6 = _w2c___comma1;
  _w2c_reg7 = i;
  _w2c_reg1 = 0;
  printf("Thread %d: Dynamic Ordered Hello World %d\n", _w2c___comma1, i);
  _w2c_i0 = i + (_INT32)(1U);
  i = _w2c_i0;
  if(_w2c_i0 <= (_INT32)(15U))
    goto _8962;
  _w2c_reg1 = 0;
  __ompc_end_serialized_parallel();
  _10754 :;
  return 0U;
} /* main */


static void __ompdo_main_1(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  
  register _UINT64 _w2c_reg6;
  register _INT32 _w2c___ompv_gtid_a0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c_reg8;
  register _INT64 _w2c_reg9;
  register _INT64 _w2c_reg10;
  register _INT32 _w2c_reg1;
  register _INT32 _w2c___ompv_temp_do_upper00;
  register _INT32 _w2c___ompv_temp_do_lower00;
  register _INT32 _w2c___mplocal_i0;
  _UINT64 _temp___slink_sym10;
  _INT32 __ompv_temp_do_upper0;
  _INT32 __ompv_temp_do_lower0;
  _INT32 __ompv_temp_do_stride0;
  
  /*Begin_of_nested_PU(s)*/
  
  _temp___slink_sym10 = _w2c_reg6;
  _w2c___ompv_gtid_a0 = (_INT32) _w2c_reg5;
  __ompv_temp_do_upper0 = 10U;
  __ompv_temp_do_lower0 = 0U;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = 2U;
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_lower0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg9 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg10 = 1U;
  * (_INT32 *)(&) = 1U;
  _w2c_reg1 = 0;
  __ompc_static_init_4(_w2c___ompv_gtid_a0, (_INT32) 2U, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0, (_INT32) 1U, (_INT32) 1U);
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  _w2c___ompv_temp_do_upper00 = (_w2c___ompv_temp_do_upper00 > (_INT32)(10U) ? 10U : _w2c___ompv_temp_do_upper00);
  __ompv_temp_do_upper0 = _w2c___ompv_temp_do_upper00;
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= _w2c___ompv_temp_do_upper00))
    goto _771;
  _1283 :;
  _w2c_reg5 = (_INT64)("Static Hello World %d\n");
  _w2c_reg6 = _w2c___mplocal_i0;
  _w2c_reg1 = 0;
  printf("Static Hello World %d\n", _w2c___mplocal_i0);
  _w2c___mplocal_i0 = _w2c___mplocal_i0 + (_INT32)(1U);
  if(__ompv_temp_do_upper0 >= _w2c___mplocal_i0)
    goto _1283;
  _771 :;
  _w2c_reg1 = 0;
  __ompc_task_exit();
  return;
} /* __ompdo_main_1 */


static void __ompdo_main_2(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  
  register _UINT64 _w2c_reg6;
  register _INT32 _w2c___ompv_gtid_a0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c_reg8;
  register _INT64 _w2c_reg9;
  register _INT64 _w2c_reg10;
  register _INT32 _w2c_reg1;
  register _INT32 _w2c___ompv_temp_do_lower00;
  register _INT32 _w2c___ompv_temp_do_upper00;
  register _INT32 _w2c___mplocal_i0;
  register _INT32 _w2c___ompv_temp_do_stride00;
  _UINT64 _temp___slink_sym11;
  _INT32 __ompv_temp_do_upper0;
  _INT32 __ompv_temp_do_lower0;
  _INT32 __ompv_temp_do_stride0;
  
  /*Begin_of_nested_PU(s)*/
  
  _temp___slink_sym11 = _w2c_reg6;
  _w2c___ompv_gtid_a0 = (_INT32) _w2c_reg5;
  __ompv_temp_do_upper0 = 10U;
  __ompv_temp_do_lower0 = 0U;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = 1U;
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_lower0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg9 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg10 = 1U;
  * (_INT32 *)(&) = *((_INT32 *) _temp___slink_sym11 + -3LL);
  _w2c_reg1 = 0;
  __ompc_static_init_4(_w2c___ompv_gtid_a0, (_INT32) 1U, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0, (_INT32) 1U, *((_INT32 *) _temp___slink_sym11 + -3LL));
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(10U)))
    goto _1283;
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  _1795 :;
  if(!(_w2c___ompv_temp_do_upper00 > (_INT32)(10U)))
    goto _3587;
  _w2c___ompv_temp_do_upper00 = 10U;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(10U)))
    goto _4099;
  __ompv_temp_do_upper0 = _w2c___ompv_temp_do_upper00;
  goto _2563;
  _3587 :;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= _w2c___ompv_temp_do_upper00))
    goto _2307;
  _2563 :;
  _2819 :;
  _w2c_reg5 = (_INT64)("Static1 Hello World %d\n");
  _w2c_reg6 = _w2c___mplocal_i0;
  _w2c_reg1 = 0;
  printf("Static1 Hello World %d\n", _w2c___mplocal_i0);
  _w2c___mplocal_i0 = _w2c___mplocal_i0 + (_INT32)(1U);
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  if(_w2c___mplocal_i0 <= _w2c___ompv_temp_do_upper00)
    goto _2819;
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  goto _2307;
  _4099 :;
  _2307 :;
  _w2c___ompv_temp_do_stride00 = __ompv_temp_do_stride0;
  _w2c___ompv_temp_do_lower00 = _w2c___ompv_temp_do_stride00 + _w2c___ompv_temp_do_lower00;
  __ompv_temp_do_lower0 = _w2c___ompv_temp_do_lower00;
  _w2c___ompv_temp_do_upper00 = _w2c___ompv_temp_do_stride00 + _w2c___ompv_temp_do_upper00;
  __ompv_temp_do_upper0 = _w2c___ompv_temp_do_upper00;
  if(_w2c___ompv_temp_do_lower00 <= (_INT32)(10U))
    goto _1795;
  _1283 :;
  _w2c_reg1 = 0;
  __ompc_task_exit();
  return;
} /* __ompdo_main_2 */


static void __ompdo_main_3(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  
  register _UINT64 _w2c_reg6;
  register _INT32 _w2c___ompv_gtid_a0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c_reg8;
  register _INT64 _w2c_reg9;
  register _INT64 _w2c_reg10;
  register _INT32 _w2c_reg1;
  register _INT32 _w2c___ompv_temp_do_lower00;
  register _INT32 _w2c___ompv_temp_do_upper00;
  register _INT32 _w2c___mplocal_i0;
  register _INT32 _w2c___ompv_temp_do_stride00;
  _UINT64 _temp___slink_sym12;
  _INT32 __ompv_temp_do_upper0;
  _INT32 __ompv_temp_do_lower0;
  _INT32 __ompv_temp_do_stride0;
  
  /*Begin_of_nested_PU(s)*/
  
  _temp___slink_sym12 = _w2c_reg6;
  _w2c___ompv_gtid_a0 = (_INT32) _w2c_reg5;
  __ompv_temp_do_upper0 = 10U;
  __ompv_temp_do_lower0 = 0U;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = 1U;
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_lower0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg9 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg10 = 1U;
  * (_INT32 *)(&) = *((_INT32 *) _temp___slink_sym12 + -4LL);
  _w2c_reg1 = 0;
  __ompc_static_init_4(_w2c___ompv_gtid_a0, (_INT32) 1U, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0, (_INT32) 1U, *((_INT32 *) _temp___slink_sym12 + -4LL));
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(10U)))
    goto _1283;
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  _1795 :;
  if(!(_w2c___ompv_temp_do_upper00 > (_INT32)(10U)))
    goto _3587;
  _w2c___ompv_temp_do_upper00 = 10U;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(10U)))
    goto _4099;
  __ompv_temp_do_upper0 = _w2c___ompv_temp_do_upper00;
  goto _2563;
  _3587 :;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= _w2c___ompv_temp_do_upper00))
    goto _2307;
  _2563 :;
  _2819 :;
  _w2c_reg5 = (_INT64)("Static2 Hello World %d\n");
  _w2c_reg6 = _w2c___mplocal_i0;
  _w2c_reg1 = 0;
  printf("Static2 Hello World %d\n", _w2c___mplocal_i0);
  _w2c___mplocal_i0 = _w2c___mplocal_i0 + (_INT32)(1U);
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  if(_w2c___mplocal_i0 <= _w2c___ompv_temp_do_upper00)
    goto _2819;
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  goto _2307;
  _4099 :;
  _2307 :;
  _w2c___ompv_temp_do_stride00 = __ompv_temp_do_stride0;
  _w2c___ompv_temp_do_lower00 = _w2c___ompv_temp_do_stride00 + _w2c___ompv_temp_do_lower00;
  __ompv_temp_do_lower0 = _w2c___ompv_temp_do_lower00;
  _w2c___ompv_temp_do_upper00 = _w2c___ompv_temp_do_stride00 + _w2c___ompv_temp_do_upper00;
  __ompv_temp_do_upper0 = _w2c___ompv_temp_do_upper00;
  if(_w2c___ompv_temp_do_lower00 <= (_INT32)(10U))
    goto _1795;
  _1283 :;
  _w2c_reg1 = 0;
  __ompc_task_exit();
  return;
} /* __ompdo_main_3 */


static void __ompdo_main_4(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  
  register _UINT64 _w2c_reg6;
  register _INT32 _w2c___ompv_gtid_a0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c_reg8;
  register _INT64 _w2c_reg9;
  register _INT64 _w2c_reg10;
  register _INT32 _w2c_reg1;
  register _INT32 _w2c_mpni_status;
  register _INT32 _w2c___ompv_temp_do_lower00;
  register _INT32 _w2c___ompv_temp_do_upper00;
  register _INT32 _w2c___mplocal_i0;
  register _INT32 _w2c___comma;
  _UINT64 _temp___slink_sym13;
  _INT32 __ompv_temp_do_upper0;
  _INT32 __ompv_temp_do_lower0;
  _INT32 __ompv_temp_do_stride0;
  
  /*Begin_of_nested_PU(s)*/
  
  _temp___slink_sym13 = _w2c_reg6;
  _w2c___ompv_gtid_a0 = (_INT32) _w2c_reg5;
  __ompv_temp_do_upper0 = 8U;
  __ompv_temp_do_lower0 = 0U;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = 3U;
  _w2c_reg7 = 0U;
  _w2c_reg8 = 8U;
  _w2c_reg9 = 1U;
  _w2c_reg10 = 1U;
  _w2c_reg1 = 0;
  __ompc_scheduler_init_4(_w2c___ompv_gtid_a0, (_INT32) 3U, (_INT32) 0U, (_INT32) 8U, (_INT32) 1U, (_INT32) 1U);
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = (_UINT64)(&__ompv_temp_do_lower0);
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg1 = 0;
  _w2c_mpni_status = __ompc_schedule_next_4(_w2c___ompv_gtid_a0, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0);
  if(!(_w2c_mpni_status))
    goto _1283;
  _1795 :;
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  if(!(_w2c___ompv_temp_do_upper00 > (_INT32)(8U)))
    goto _3587;
  __ompv_temp_do_upper0 = 8U;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(8U)))
    goto _4099;
  goto _2563;
  _3587 :;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= _w2c___ompv_temp_do_upper00))
    goto _2307;
  _2563 :;
  _2819 :;
  _w2c___comma = omp_get_thread_num();
  _w2c_reg5 = (_INT64)("Thread %d: Dynamic Hello World %d\n");
  _w2c_reg6 = _w2c___comma;
  _w2c_reg7 = _w2c___mplocal_i0;
  _w2c_reg1 = 0;
  printf("Thread %d: Dynamic Hello World %d\n", _w2c___comma, _w2c___mplocal_i0);
  _w2c___mplocal_i0 = __ompv_temp_do_stride0 + _w2c___mplocal_i0;
  if(__ompv_temp_do_upper0 >= _w2c___mplocal_i0)
    goto _2819;
  goto _2307;
  _4099 :;
  _2307 :;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = (_UINT64)(&__ompv_temp_do_lower0);
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg1 = 0;
  _w2c_mpni_status = __ompc_schedule_next_4(_w2c___ompv_gtid_a0, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0);
  if(_w2c_mpni_status)
    goto _1795;
  _1283 :;
  _w2c_reg1 = 0;
  __ompc_task_exit();
  return;
} /* __ompdo_main_4 */


static void __ompdo_main_5(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  
  register _UINT64 _w2c_reg6;
  register _INT32 _w2c___ompv_gtid_a0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c_reg8;
  register _INT64 _w2c_reg9;
  register _INT64 _w2c_reg10;
  register _INT32 _w2c_reg1;
  register _INT32 _w2c_mpni_status;
  register _INT32 _w2c___ompv_temp_do_lower00;
  register _INT32 _w2c___ompv_temp_do_upper00;
  register _INT32 _w2c___mplocal_i0;
  register _INT32 _w2c___comma;
  _UINT64 _temp___slink_sym14;
  _INT32 __ompv_temp_do_upper0;
  _INT32 __ompv_temp_do_lower0;
  _INT32 __ompv_temp_do_stride0;
  
  /*Begin_of_nested_PU(s)*/
  
  _temp___slink_sym14 = _w2c_reg6;
  _w2c___ompv_gtid_a0 = (_INT32) _w2c_reg5;
  __ompv_temp_do_upper0 = 9U;
  __ompv_temp_do_lower0 = 0U;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = 34U;
  _w2c_reg7 = 0U;
  _w2c_reg8 = 9U;
  _w2c_reg9 = 1U;
  _w2c_reg10 = 1U;
  _w2c_reg1 = 0;
  __ompc_scheduler_init_4(_w2c___ompv_gtid_a0, (_INT32) 34U, (_INT32) 0U, (_INT32) 9U, (_INT32) 1U, (_INT32) 1U);
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = (_UINT64)(&__ompv_temp_do_lower0);
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg1 = 0;
  _w2c_mpni_status = __ompc_schedule_next_4(_w2c___ompv_gtid_a0, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0);
  if(!(_w2c_mpni_status))
    goto _1283;
  _1795 :;
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  if(!(_w2c___ompv_temp_do_upper00 > (_INT32)(9U)))
    goto _3587;
  __ompv_temp_do_upper0 = 9U;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(9U)))
    goto _4099;
  goto _2563;
  _3587 :;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= _w2c___ompv_temp_do_upper00))
    goto _2307;
  _2563 :;
  _2819 :;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg1 = 0;
  __ompc_ordered(_w2c___ompv_gtid_a0);
  _w2c___comma = omp_get_thread_num();
  _w2c_reg5 = (_INT64)("Thread %d: Static Ordered Hello World %d\n");
  _w2c_reg6 = _w2c___comma;
  _w2c_reg7 = _w2c___mplocal_i0;
  _w2c_reg1 = 0;
  printf("Thread %d: Static Ordered Hello World %d\n", _w2c___comma, _w2c___mplocal_i0);
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg1 = 0;
  __ompc_end_ordered(_w2c___ompv_gtid_a0);
  _w2c___mplocal_i0 = __ompv_temp_do_stride0 + _w2c___mplocal_i0;
  if(__ompv_temp_do_upper0 >= _w2c___mplocal_i0)
    goto _2819;
  goto _2307;
  _4099 :;
  _2307 :;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = (_UINT64)(&__ompv_temp_do_lower0);
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg1 = 0;
  _w2c_mpni_status = __ompc_schedule_next_4(_w2c___ompv_gtid_a0, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0);
  if(_w2c_mpni_status)
    goto _1795;
  _1283 :;
  _w2c_reg1 = 0;
  __ompc_task_exit();
  return;
} /* __ompdo_main_5 */


static void __ompdo_main_6(__ompv_gtid_a, __ompv_slink_a)
  _INT32 __ompv_gtid_a;
  _UINT64 __ompv_slink_a;
{
  
  register _UINT64 _w2c_reg6;
  register _INT32 _w2c___ompv_gtid_a0;
  register _INT64 _w2c_reg5;
  register _INT64 _w2c_reg7;
  register _INT64 _w2c_reg8;
  register _INT64 _w2c_reg9;
  register _INT64 _w2c_reg10;
  register _INT32 _w2c_reg1;
  register _INT32 _w2c_mpni_status;
  register _INT32 _w2c___ompv_temp_do_lower00;
  register _INT32 _w2c___ompv_temp_do_upper00;
  register _INT32 _w2c___mplocal_i0;
  register _INT32 _w2c___comma;
  _UINT64 _temp___slink_sym15;
  _INT32 __ompv_temp_do_upper0;
  _INT32 __ompv_temp_do_lower0;
  _INT32 __ompv_temp_do_stride0;
  
  /*Begin_of_nested_PU(s)*/
  
  _temp___slink_sym15 = _w2c_reg6;
  _w2c___ompv_gtid_a0 = (_INT32) _w2c_reg5;
  __ompv_temp_do_upper0 = 15U;
  __ompv_temp_do_lower0 = 0U;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = 35U;
  _w2c_reg7 = 0U;
  _w2c_reg8 = 15U;
  _w2c_reg9 = 1U;
  _w2c_reg10 = 1U;
  _w2c_reg1 = 0;
  __ompc_scheduler_init_4(_w2c___ompv_gtid_a0, (_INT32) 35U, (_INT32) 0U, (_INT32) 15U, (_INT32) 1U, (_INT32) 1U);
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = (_UINT64)(&__ompv_temp_do_lower0);
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg1 = 0;
  _w2c_mpni_status = __ompc_schedule_next_4(_w2c___ompv_gtid_a0, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0);
  if(!(_w2c_mpni_status))
    goto _1283;
  _1795 :;
  _w2c___ompv_temp_do_lower00 = __ompv_temp_do_lower0;
  _w2c___ompv_temp_do_upper00 = __ompv_temp_do_upper0;
  if(!(_w2c___ompv_temp_do_upper00 > (_INT32)(15U)))
    goto _3587;
  __ompv_temp_do_upper0 = 15U;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= (_INT32)(15U)))
    goto _4099;
  goto _2563;
  _3587 :;
  _w2c___mplocal_i0 = _w2c___ompv_temp_do_lower00;
  if(!(_w2c___ompv_temp_do_lower00 <= _w2c___ompv_temp_do_upper00))
    goto _2307;
  _2563 :;
  _2819 :;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg1 = 0;
  __ompc_ordered(_w2c___ompv_gtid_a0);
  _w2c___comma = omp_get_thread_num();
  _w2c_reg5 = (_INT64)("Thread %d: Dynamic Ordered Hello World %d\n");
  _w2c_reg6 = _w2c___comma;
  _w2c_reg7 = _w2c___mplocal_i0;
  _w2c_reg1 = 0;
  printf("Thread %d: Dynamic Ordered Hello World %d\n", _w2c___comma, _w2c___mplocal_i0);
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg1 = 0;
  __ompc_end_ordered(_w2c___ompv_gtid_a0);
  _w2c___mplocal_i0 = __ompv_temp_do_stride0 + _w2c___mplocal_i0;
  if(__ompv_temp_do_upper0 >= _w2c___mplocal_i0)
    goto _2819;
  goto _2307;
  _4099 :;
  _2307 :;
  _w2c_reg5 = _w2c___ompv_gtid_a0;
  _w2c_reg6 = (_UINT64)(&__ompv_temp_do_lower0);
  _w2c_reg7 = (_INT64)(&__ompv_temp_do_upper0);
  _w2c_reg8 = (_INT64)(&__ompv_temp_do_stride0);
  _w2c_reg1 = 0;
  _w2c_mpni_status = __ompc_schedule_next_4(_w2c___ompv_gtid_a0, &__ompv_temp_do_lower0, &__ompv_temp_do_upper0, &__ompv_temp_do_stride0);
  if(_w2c_mpni_status)
    goto _1795;
  _1283 :;
  _w2c_reg1 = 0;
  __ompc_task_exit();
  return;
} /* __ompdo_main_6 */

