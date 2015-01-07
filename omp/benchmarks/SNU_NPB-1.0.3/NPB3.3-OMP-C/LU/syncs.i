# 1 "syncs.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "syncs.c"
# 35 "syncs.c"
# 1 "applu.incl" 1
# 12 "applu.incl"
# 1 "npbparams.h" 1
# 13 "applu.incl" 2
# 1 "../common/type.h" 1



typedef enum { false, true } logical;
typedef struct {
  double real;
  double imag;
} dcomplex;
# 14 "applu.incl" 2
# 40 "applu.incl"
extern double dxi, deta, dzeta;
extern double tx1, tx2, tx3;
extern double ty1, ty2, ty3;
extern double tz1, tz2, tz3;
extern int nx, ny, nz;
extern int nx0, ny0, nz0;
extern int ist, iend;
extern int jst, jend;
extern int ii1, ii2;
extern int ji1, ji2;
extern int ki1, ki2;





extern double dx1, dx2, dx3, dx4, dx5;
extern double dy1, dy2, dy3, dy4, dy5;
extern double dz1, dz2, dz3, dz4, dz5;
extern double dssp;
# 69 "applu.incl"
extern double u [12][12/2*2+1][12/2*2+1][5];
extern double rsd [12][12/2*2+1][12/2*2+1][5];
extern double frct [12][12/2*2+1][12/2*2+1][5];
extern double flux [12][5];
extern double qs [12][12/2*2+1][12/2*2+1];
extern double rho_i[12][12/2*2+1][12/2*2+1];





extern int ipr, inorm;





extern double dt, omega, tolrsd[5], rsdnm[5], errnm[5], frc, ttotal;
extern int itmax, invert;


extern double a[12][12/2*2+1][5][5];
extern double b[12][12/2*2+1][5][5];
extern double c[12][12/2*2+1][5][5];
extern double d[12][12/2*2+1][5][5];


extern double au[12][12/2*2+1][5][5];
extern double bu[12][12/2*2+1][5][5];
extern double cu[12][12/2*2+1][5][5];
extern double du[12][12/2*2+1][5][5];






extern double ce[5][13];






extern int isync[12 +1];

extern int mthreadnum, iam;
#pragma omp threadprivate(mthreadnum,iam)






extern double maxtime;
extern logical timeron;
# 139 "applu.incl"
void read_input();
void domain();
void setcoeff();
void setbv();
void exact(int i, int j, int k, double u000ijk[]);
void setiv();
void erhs();
void ssor(int niter);
void rhs();
void l2norm (int ldx, int ldy, int ldz, int nx0, int ny0, int nz0,
     int ist, int iend, int jst, int jend,
     double v[][ldy/2*2+1][ldx/2*2+1][5], double sum[5]);
void jacld(int k);
void blts(int ldmx, int ldmy, int ldmz, int nx, int ny, int nz, int k,
    double omega,
    double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5],
    double ldz[ldmy][ldmx/2*2+1][5][5],
    double ldy[ldmy][ldmx/2*2+1][5][5],
    double ldx[ldmy][ldmx/2*2+1][5][5],
    double d[ldmy][ldmx/2*2+1][5][5],
    int ist, int iend, int jst, int jend, int nx0, int ny0);
void jacu(int k);
void buts(int ldmx, int ldmy, int ldmz, int nx, int ny, int nz, int k,
    double omega,
    double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5],
    double tv[ldmy][ldmx/2*2+1][5],
    double d[ldmy][ldmx/2*2+1][5][5],
    double udx[ldmy][ldmx/2*2+1][5][5],
    double udy[ldmy][ldmx/2*2+1][5][5],
    double udz[ldmy][ldmx/2*2+1][5][5],
    int ist, int iend, int jst, int jend, int nx0, int ny0);
void error();
void sync_left(int ldmx, int ldmy, int ldmz,
               double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5]);
void sync_right(int ldmx, int ldmy, int ldmz,
                double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5]);
void pintgr();
void verify(double xcr[5], double xce[5], double xci,
            char *Class, logical *verified);
# 36 "syncs.c" 2




void sync_left(int ldmx, int ldmy, int ldmz,
               double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5])
{
  int neigh;

  if (iam > 0 && iam <= mthreadnum) {
    neigh = iam - 1;
    while (isync[neigh] == 0) {
#pragma omp flush(isync)
    }
    isync[neigh] = 0;
#pragma omp flush(isync,v)
  }
}





void sync_right(int ldmx, int ldmy, int ldmz,
                double v[ldmz][ldmy/2*2+1][ldmx/2*2+1][5])
{
  if (iam < mthreadnum) {
#pragma omp flush(isync,v)
    while (isync[iam] == 1) {
#pragma omp flush(isync)
    }
    isync[iam] = 1;
#pragma omp flush(isync)
  }
}
