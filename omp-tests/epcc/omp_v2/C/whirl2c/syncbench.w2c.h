/* Include builtin types and operators */
#include <whirl2c.h>

/* Types */
struct anonymous21 {
    _INT32 __m_reserved;
    _INT32 __m_count;
    struct _pthread_descr_struct * __m_owner;
    _INT32 __m_kind;
    struct _pthread_fastlock __m_lock;
  };
  /* File-level vars and routines */
struct _IO_FILE_plus {
  _UINT8 dummy;
};
struct _IO_FILE {
  _INT32 _flags;
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

_IEEE64 times[21LL];

_IEEE64 reftime;

_IEEE64 refsd;

extern _INT32 main(_INT32, _INT8 **);

extern _INT32 omp_get_num_threads(void);

extern _INT32 printf(const _INT8 *, ...);

extern void refer(void);

extern void testpr(void);

extern void testfor(void);

extern void testpfor(void);

extern void testbar(void);

extern void testsing(void);

extern void testcrit(void);

extern void testlock(void);

extern void testorder(void);

extern void referatom(void);

extern void testatom(void);

extern void referred(void);

extern void testred(void);

extern _IEEE64 getclock(void);

extern void delay(_INT32);

extern void stats(_IEEE64 *, _IEEE64 *);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern _IEEE64 getclock(void);

extern void omp_init_lock(volatile struct anonymous21 *);

extern _IEEE64 getclock(void);

extern void omp_set_lock(volatile struct anonymous21 *);

extern void omp_unset_lock(volatile struct anonymous21 *);

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

static void __ompregion_testpr1();

extern _INT32 __ompc_critical();

static void __ompregion_testfor1();

extern _INT32 __ompc_static_init_4();

extern _INT32 __ompc_barrier();

static void __ompdo_testpfor1();

static void __ompregion_testbar1();

static void __ompregion_testsing1();

extern _INT32 __ompc_single();

extern _INT32 __ompc_end_single();

_INT64 __mplock_0;

extern _INT32 __ompc_end_critical();

static void __ompregion_testcrit1();

static void __ompregion_testlock1();

static void __ompdo_testorder1();

extern _INT32 __ompc_ordered();

extern _INT32 __ompc_end_ordered();

extern _INT32 __ompc_scheduler_init_4();

extern _INT32 __ompc_schedule_next_4();

_INT64 __namelock___OMP_CRITICAL_ATOMIC_F4;

static void __ompregion_testatom1();

static void __ompregion_testred1();

_INT64 __mplock_1;

