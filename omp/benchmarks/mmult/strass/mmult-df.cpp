#include "block.h"
#include <cstdlib>
#include <vector>

using std::vector;

vector<double> A, B, C, R1, R2;

void init_data_matrix(vector<double> &V, int size) {
    V.reserve(size*size);

    for(int i=0; i < size*size; i++) {
        V[i] = std::rand();
    }
}

void init_result_matrix( vector<double> &R, int size) {
    R.reserve(size*size);
    for(auto entry : R ) {
        entry = 0;
    }
}

void init(int size) {
    std::srand(0);
    init_data_matrix(A, size);
    init_data_matrix(B, size);
    init_data_matrix(C, size);

    init_result_matrix(R1, size);
    init_result_matrix(R2, size);

}

serial_mmult(block 

block** mmult( block **blockList, int numBlocks, 
               vector<double> &result,
               vector<double> &input1,
               vector<double> &input2 ){

    int numBlocks = size/blocksize;

    for(int i = 0; i < numBlocks; i++) {
        for(int j = 0; j < numBlocks; j++) {
            
        }
    }

    return blockList;
}

int main(int argc, char **argv) 
{
    //for now, everything is square
    int size = 1024;
    int blocksize = 64;
    if(argc > 1)
        size = atoi(argv[1]);
    if(argc > 2)
        blocksize = atoi(argv[2]);

    init(size);
    block **blA = getBlockList(size, blocksize);
    block **blB = getBlockList(size, blocksize);
    block **blC = getBlockList(size, blocksize);
    block **blR1 = getBlockList(size, blocksize);
    block **blR2 = getBlockList(size, blocksize);

    //this is messy. I need to tie the matrix and it's blockList together better.
    //mmult(blockList, size/blocksize, R1, A, B);
    
    blockList = mmult(blockList, size/blocksize, R2, R1, C);


}
