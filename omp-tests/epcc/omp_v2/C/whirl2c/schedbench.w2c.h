/* Include builtin types and operators */
#include <whirl2c.h>

/* Types */
/* File-level vars and routines */
struct _IO_FILE_plus {
  _UINT8 dummy;
};
struct _IO_FILE {
  _INT32 _flags;
  _UINT8 fill0[4LL];
  _INT8 * _IO_read_ptr;
  _INT8 * _IO_read_end;
  _INT8 * _IO_read_base;
  _INT8 * _IO_write_base;
  _INT8 * _IO_write_ptr;
  _INT8 * _IO_write_end;
  _INT8 * _IO_buf_base;
  _INT8 * _IO_buf_end;
  _INT8 * _IO_save_base;
  _INT8 * _IO_backup_base;
  _INT8 * _IO_save_end;
  struct _IO_marker * _markers;
  struct _IO_FILE * _chain;
  _INT32 _fileno;
  _INT32 _flags2;
  _INT64 _old_offset;
  _UINT16 _cur_column;
  _INT8 _vtable_offset;
  _INT8 _shortbuf[1LL];
  _UINT8 fill[4LL];
  void * _lock;
  _INT64 _offset;
  void * __pad1;
  void * __pad2;
  _INT32 _mode;
  _INT8 _unused2[44LL];
};
_INT32 nthreads;

_INT32 delaylength;

_INT32 innerreps;

_INT32 itersperthr;

_INT32 cksz;

_IEEE64 times[21LL];

_IEEE64 reftime;

_IEEE64 refsd;

extern _INT32 main(_INT32, _INT8 **);

extern _INT32 omp_get_num_threads(void);

extern _INT32 printf(const _INT8 *, ...);

extern void getdelay(void);

extern void refer(void);

extern void teststatic(void);

extern void teststaticn(void);

extern void testdynamicn(void);

extern void testguidedn(void);

extern void delay(_INT32);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern void stats(_IEEE64 *, _IEEE64 *);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 sqrt(_IEEE64);

static void __ompregion_main1();

extern _INT32 __ompc_master();

extern _INT32 __ompc_fork();

extern _INT32 __ompc_get_local_thread_num();

extern _INT32 __ompc_serialized_parallel();

extern _INT32 __ompc_end_serialized_parallel();

extern _INT32 __ompc_can_fork();

static void __ompregion_teststatic1();

extern _INT32 __ompc_static_init_4();

extern _INT32 __ompc_barrier();

static void __ompregion_teststaticn1();

static void __ompregion_testdynamicn1();

extern _INT32 __ompc_scheduler_init_4();

extern _INT32 __ompc_schedule_next_4();

static void __ompregion_testguidedn1();

