
//  Copyright (c) 2011-2013 Thomas Heller
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include "jacobi.hpp"

#include <hpx/hpx.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>
#include <hpx/lcos/local/dataflow.hpp>
#include <hpx/util/high_resolution_timer.hpp>
#include <hpx/util/unwrapped.hpp>

using boost::shared_ptr;
using std::min;
using std::vector;
using hpx::make_ready_future;
using hpx::lcos::local::dataflow;
using hpx::lcos::shared_future;

using hpx::util::unwrapped;

struct block {
    double *src;
    double *dest;
    size_t matrix_size;
    size_t block_size;
    size_t col;
    size_t row;
};

namespace jacobi_smp {

    void jacobi_kernel_wrapper(range const & y_range, size_t n, vector<double> & dst, vector<double> const & src) {
        for(size_t y = y_range.begin(); y < y_range.end(); ++y) {
                  double * dst_ptr = &dst[y * n];
            const double * src_ptr = &src[y * n];
            jacobi_kernel( dst_ptr, src_ptr, n );
        }
    }
    
    block jacobi_kernel_mid  (block previous, block left, block right, block below, block above);
    block jacobi_kernel_top  (block previous, block left, block right, block below             );
    block jacobi_kernel_bot  (block previous, block left, block right,              block above);
    block jacobi_kernel_left (block previous,             block right, block below, block above);
    block jacobi_kernel_right(block previous, block left,              block below, block above);
    block jacobi_kernel_TL   (block previous,             block right, block below             );
    block jacobi_kernel_TR   (block previous, block left,              block below             );
    block jacobi_kernel_BL   (block previous,             block right,              block above);
    block jacobi_kernel_BR   (block previous, block left,                           block above);


    auto jacobi_op    = unwrapped(&jacobi_kernel_mid);
    auto jacobi_bot   = unwrapped(&jacobi_kernel_bot);
    auto jacobi_top   = unwrapped(&jacobi_kernel_top);
    auto jacobi_left  = unwrapped(&jacobi_kernel_left);
    auto jacobi_right = unwrapped(&jacobi_kernel_right);
    auto jacobi_BL    = unwrapped(&jacobi_kernel_BL);
    auto jacobi_BR    = unwrapped(&jacobi_kernel_BR);
    auto jacobi_TL    = unwrapped(&jacobi_kernel_TL);
    auto jacobi_TR    = unwrapped(&jacobi_kernel_TR);

    void jacobi_init(vector< vector< vector< hpx::shared_future<block> > > > &futureList, size_t n, size_t block_size) {
        size_t numBlocks = static_cast<size_t>(std::ceil(double(n)/block_size));
        futureList[0].resize(numBlocks);
        futureList[1].resize(numBlocks);

        vector< vector<block> > blockList;
        blockList.resize(numBlocks);

        for(int i = 0; i < numBlocks; i++){
            futureList[0][i].resize(numBlocks);
            futureList[1][i].resize(numBlocks);
            blockList[i].resize(numBlocks);
        }

        
        const size_t curr = 1;
        
        futureList[1][0][0] = hpx::async(
                jacobi_kernel_BL, blockList[0][0],
                           blockList[0][1],
                           blockList[1][0] );

        for(size_t i = 1; i < numBlocks - 1; i++) {
            for(size_t j = 1; j < numBlocks - 1; j++) {
            futureList[curr][i][j] = hpx::async(
                    jacobi_kernel_mid, blockList[i  ][j  ],
                                       blockList[i-1][j  ],
                                       blockList[i+1][j  ],
                                       blockList[i  ][j-1],
                                       blockList[i  ][j+1] );
            }
        }


        //allocate matrices
        //initialize block List
    }

    void jacobi( size_t n , size_t iterations, size_t block_size, std::string output_filename) {
        vector< vector< vector< shared_future<block> > > > blockList(2);
        jacobi_init(blockList, n, block_size);

        hpx::util::high_resolution_timer t;
        for(size_t i = 1; i < iterations; ++i) {
            const size_t prev = i%2;
            const size_t curr = (i+1)%2;
            blockList[curr][0][0] = dataflow(
                    jacobi_BL, blockList[prev][0][0],
                               blockList[prev][0][1],
                               blockList[prev][1][0] );
            for(size_t j = 1; j < blockList[i%2].size() - 1; j++) {
                blockList[curr][j][0] = dataflow(
                        jacobi_left, blockList[prev][j  ][0],
                                     blockList[prev][j  ][1],
                                     blockList[prev][j+1][0],
                                     blockList[prev][j+1][1] );
            }
            //TL
            for(size_t j = 1; j < blockList[i%2].size() - 1; j++) {
                //bot
                for(size_t k = 1; k < blockList[i%2][j].size() - 1; k++) {
                    blockList[curr][j][k] = dataflow( 
                            jacobi_op, blockList[prev][j  ][k  ],
                                       blockList[prev][j-1][k  ],
                                       blockList[prev][j+1][k  ],
                                       blockList[prev][j  ][k-1],
                                       blockList[prev][j  ][k+1]);
                }
                //top
            }
            //BR
            for(size_t j = 1; j < blockList[i%2].size() - 1; j++) {
                //right
            }
            //TR
        }
        for(int i = 0; i < blockList[(n-1)%2].size(); i++) {
            hpx::wait_all(blockList[(n-1)%2][i]);
        }

        report_timing(n, iterations, t.elapsed());
        //output_grid(output_filename, *grid_old, n);
   }
}
