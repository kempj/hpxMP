// Static blocked LU Decomposition
#include <stdio.h>
#include <cstdlib>
#include "lu-local.h"

using std::vector;

vector<double> A;


//TODO: how does pass by reference to a future work?
#pragma omp future blockList //?
void init_df(vector<vector<vector<block>>> &blockList, int numBlocks, int size) {
    
//this 'waits' on blockList[0] and calls the following function synchronously.
#pragma omp future wait(blockList[0])
    getBlockList(blockList[0], numBlocks, size);
    //what about calling a method on a future object?
    //Or resizing them in general?
    //if the number of dimensions is known at declaration, then the resizing can be done normally,
    blockList[1].resize(numBlocks);

    for(int i = 0; i < numBlocks; i++){
        blockList[1][i].resize( numBlocks );
    }

#pragma omp task future( in: blockList[0][0][0] ) \
                 future(out: blockList[0][0][0] )
    blockList[0][0][0] = diag_op( size, blockList[0][0][0] );
    for(int i = 1; i < numBlocks; i++) {
#pragma omp task future( in: blockList[0][0][0], blockList[0][0][i] ) \
                 future(out: blockList[0][0][i] )
        blockList[0][0][i] = row_op( size, blockList[0][0][i], 
                                           blockList[0][0][0] );
    }
    for(int i = 1; i < numBlocks; i++) {
#pragma omp task future( in: blockList[0][0][0], blockList[0][i][0] ) \
                 future(out: blockList[0][i][0] )
        blockList[0][i][0] = col_op( size, blockList[0][i][0], 
                                           blockList[0][0][0] );
        for(int j = 1; j < numBlocks; j++) {
#pragma omp task future( in: blockList[0][i][j], blockList[0][0][j] ), \
                             blockList[0][i][0] \
                 future(out: blockList[0][i][j] )
            blockList[0][i][j] = inner_op( size, blockList[0][i][j],
                                                 blockList[0][0][j],
                                                 blockList[0][i][0] );
        }
    }
}

void LU( int size, int numBlocks)
{
    //TODO: what size needs to be known when declaring this? 
    //If arrays of futures are to be able to be dynamically allocated, at least the number of
    //dimensions must be known now.
//#pragma omp future blockList //no
#pragma omp future blockList[2][numBlocks][numBlocks] //?
    vector<vector<vector<block>>> blockList(2);

    //because this is not a task, it is called synchronously, and passes blockList as a future
    init_df(blockList, numBlocks, size);

    for(int i = 1; i < numBlocks; i++) {
#pragma omp task future( in: blockList[(i-1)%2][i][i] ) \
                 future(out: blockList[i%2][i][i] )
        blockList[i%2][i][i] = diag_op( size, blockList[(i-1)%2][i][i] );
        for(int j = i + 1; j < numBlocks; j++){
#pragma omp task future( in: blockList[(i-1)%2][i][j], blockList[i%2][i][i] ) \
                 future(out: blockList[i%2][i][j] )
            blockList[i%2][i][j] = row_op( size, blockList[(i-1)%2][i][j],
                                                 blockList[ i   %2][i][i] );
        }
        for(int j = i + 1; j < numBlocks; j++){
#pragma omp task future( in: blockList[(i-1)%2][j][i], blockList[i%2][i][i] ) \
                 future(out: blockList[i%2][j][i] )
            blockList[i%2][j][i] = col_op( size, blockList[(i-1)%2][j][i], 
                                                 blockList[ i   %2][i][i] );
            for(int k = i + 1; k < numBlocks; k++) {
#pragma omp task future( in: blockList[(i-1)%2][j][k], blockList[i%2][i][k], \
                             blockList[i%2][j][i] ) \
                 future(out: blockList[i%2][j][k] )
                blockList[i%2][j][k] = inner_op( size, blockList[(i-1)%2][j][k], 
                                                       blockList[ i   %2][i][k],
                                                       blockList[ i   %2][j][i] );
            }
        }
    }
#pragma omp future wait blockList[(numBlocks-1)%2][numBlocks-1][numBlocks-1];
}

int main(int argc, char *argv[])
{
    vector<double> originalA;
    int size = 1000;
    int numBlocks = 10;
    unsigned long t1, t2;
    bool runCheck = false;

    if( argc > 1 )
        size = atoi(argv[1]);
    if( argc > 2 )
        numBlocks = atoi(argv[2]);
    if( argc > 3 )
        runCheck = true;
    printf("size = %d, numBlocks = %d\n", size, numBlocks);

    A.resize(size*size, 0);
    InitMatrix3( size );
    if(runCheck) {
        printf("Error checking enabled\n");
        originalA.reserve(size*size);
        for(int i = 0; i < size * size; i++) {
            originalA[i] = A[i];
        }
    }

    if(numBlocks == 1) {
        t1 = GetTickCount();
        diag_op( size, block(size, 0, size));
        t2 = GetTickCount();
    } else if( numBlocks > 1) {
        t1 = GetTickCount();
        LU( size, numBlocks);
        t2 = GetTickCount();
    } else { 
        printf("Error: numBlocks must be greater than 0.\n");
        return 0;
    }
    printf("Time for LU-decomposition in secs: %f \n", (t2-t1)/1000000.0);
    
    if(runCheck) {
        checkResult( originalA,  size );
    }
    return 0;
}
