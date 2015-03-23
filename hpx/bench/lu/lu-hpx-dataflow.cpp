// Static blocked LU Decomposition

#include <stdio.h>
#include <hpx/hpx_init.hpp>
#include <hpx/include/threads.hpp>
#include <hpx/include/lcos.hpp>
#include <hpx/lcos/local/dataflow.hpp>
#include <hpx/util/unwrapped.hpp>

#include "lu-local.h"

using std::vector;
using hpx::util::unwrapped;
using hpx::lcos::shared_future;
using hpx::lcos::wait_all;
using hpx::async;
using hpx::lcos::local::dataflow;
using hpx::when_all;
using hpx::make_ready_future;


vector<double> A;
auto diag_op  = unwrapped( &ProcessDiagonalBlock );
auto row_op   = unwrapped( &ProcessBlockOnRow );
auto col_op   = unwrapped( &ProcessBlockOnColumn );
auto inner_op = unwrapped( &ProcessInnerBlock );


void init_df(vector<vector<vector<shared_future<block>>>> &dfArray, int numBlocks, int size) {
    vector<vector<block>> blockList;
    getBlockList(blockList, numBlocks, size);

    for(int i = 0; i < numBlocks; i++){
        dfArray[i].resize( numBlocks );
        for(int j = 0; j < numBlocks; j++){
            dfArray[i][j].resize( numBlocks, make_ready_future( block()));
        }
    }

    shared_future<int> fsize = make_ready_future(size);

    dfArray[0][0][0] = async( ProcessDiagonalBlock, size, blockList[0][0] );
    auto *diag_block = &dfArray[0][0][0];
    for(int i = 1; i < numBlocks; i++) {
        dfArray[0][0][i] = dataflow( row_op, fsize, make_ready_future( blockList[0][i] ), *diag_block);
    }
    for(int i = 1; i < numBlocks; i++) {
        dfArray[0][i][0] = dataflow( col_op, fsize, make_ready_future( blockList[i][0] ), *diag_block);
        auto *first_col = &dfArray[0][i][0];
        for(int j = 1; j < numBlocks; j++) {
            dfArray[0][i][j] = dataflow( inner_op, fsize, make_ready_future( blockList[i][j]), dfArray[0][0][j], *first_col );
        }
    }
}

void LU( int size, int numBlocks)
{
    vector<vector<vector<shared_future<block>>>> dfArray(numBlocks);
    init_df(dfArray, numBlocks, size);
    shared_future<block> *diag_block, *first_col;
    shared_future<int> fsize = make_ready_future(size);

    for(int i = 1; i < numBlocks; i++) {
        //TODO: use two 2d df arrays, instead of a 3d df array
        dfArray[i][i][i] = dataflow( diag_op, fsize, dfArray[i-1][i][i]);
        diag_block = &dfArray[i][i][i];
        for(int j = i + 1; j < numBlocks; j++){
            dfArray[i][i][j] = dataflow( row_op , fsize, dfArray[i-1][i][j], *diag_block);
        }
        for(int j = i + 1; j < numBlocks; j++){
            dfArray[i][j][i] = dataflow( col_op, fsize, dfArray[i-1][j][i], *diag_block);
            first_col = &dfArray[i][j][i];
            for(int k = i + 1; k < numBlocks; k++) {
                dfArray[i][j][k] = dataflow( inner_op, fsize, dfArray[i-1][j][k], dfArray[i][i][k], *first_col );
            }
        }
    }
    wait_all(dfArray[numBlocks-1][numBlocks-1][numBlocks-1]);
}

int hpx_main(int argc, char *argv[])
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

    t1 = GetTickCount();
    if(numBlocks == 1) {
        ProcessDiagonalBlock( size, block(size, 0, size));
    } else if( numBlocks > 1) {
        LU( size, numBlocks);
    } else { 
        printf("Error: numBlocks must be greater than 0.\n");
    }
    t2 = GetTickCount();
    printf("Time for LU-decomposition in secs: %f \n", (t2-t1)/1000000.0);
    
    if(runCheck) {
        checkResult( originalA,  size );
    }
    return hpx::finalize();
}

int main(int argc, char *argv[])
{
    using namespace boost::assign;
    std::vector<std::string> cfg;
    cfg += "hpx.os_threads=" +
        boost::lexical_cast<std::string>(hpx::threads::hardware_concurrency());

    return hpx::init(argc, argv, cfg);
}
