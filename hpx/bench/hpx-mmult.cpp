#include <hpx/hpx_init.hpp>
#include <hpx/runtime/threads/topology.hpp>
#include <boost/format.hpp>

#include <matrix_block.h>

using hpx::lcos::shared_future;
using hpx::lcos::future;
using std::vector;
using std::cout;
using std::endl;


const int blocksize = 64;


block serial_mult(block A, block B, block C) {
    for (int i = 0; i < C.width; i++) {
        for (int j = 0; j < C.height; j++) {
            for (int k = 0; k < A.width;k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
    return C;
}

block add_blocks(block A, block B, block result) {
    for(int i = 0; i < A.width; i++){
        for(int j = 0; j < A.height; j++) {
            result[i][j] = A[i][j] + B[i][j];
        }
    }
    return result;
}

block calc_c11(block A, block B, block C) {
    block tempC(C.block11());
    block A11B11 = rec_mult(A.block11(), B.block11(), C.block11());
    block A12B21 = rec_mult(A.block12(), B.block21(), tempC);
    return add_blocks(A11B11, A12B21, C.block11());
}

block calc_c12(block A, block B, block C) {
    block tempC(C.block12());
    block A11B12 = rec_mult(A.block11(), B.block12(), C.block12());
    block A12B22 = rec_mult(A.block12(), B.block22(), tempC);
    return add_blocks(A11B12, A12B22, C.block12());
}

block calc_c21(block A, block B, block C) {
    block tempC(C.block21());
    block A21B11 = rec_mult(A.block21(), B.block11(), C.block21());
    block A22B21 = rec_mult(A.block22(), B.block21(), tempC);
    return add_blocks(A21B11, A22B21, C.block21());
}

block calc_c22(block A, block B, block C) {
    block tempC(C.block22());
    block A21B12 = rec_mult(A.block21(), B.block12(), C.block22());
    block A22B22 = rec_mult(A.block22(), B.block22(), tempC);
    return add_blocks(A21B12, A22B22, C.block22());
}

block rec_mult(block A, block B, block C) {
    if(C.width < blocksize || C.height < blocksize ) {
        return serial_mult(A, B, C);
    } 
    C11 = calc_c11(A, B, C);
    C12 = calc_c12(A, B, C);
    C21 = calc_c21(A, B, C);
    C22 = calc_c22(A, B, C);
    return C;
}


int hpx_main(boost::program_options::variables_map& vm) {
    int niter = 1, N = 1000, block_size = 128;
    srand((unsigned long)time(NULL));

    block a(N*N);
    block b(N*N);
    block c(new double[N*N]{0}, N);

    rec_mult(a, b, c);

    return hpx::finalize();
}

int main(int argc, char ** argv) {

    hpx::init(argc, argv);

    return 0;
}
