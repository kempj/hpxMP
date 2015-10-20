
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

//using hpx::util::unwrapped;

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
    
    void jacobi_kernel_mid  (block previous, block left, block right, block below, block above);
    void jacobi_kernel_top  (block previous, block left, block right, block below             );
    void jacobi_kernel_bot  (block previous, block left, block right,              block above);
    void jacobi_kernel_left (block previous,             block right, block below, block above);
    void jacobi_kernel_right(block previous, block left,              block below, block above);
    void jacobi_kernel_TL   (block previous,             block right, block below             );
    void jacobi_kernel_TR   (block previous, block left,              block below             );
    void jacobi_kernel_BL   (block previous,             block right,              block above);
    void jacobi_kernel_BR   (block previous, block left,                           block above);


    auto jacobi_op =   hpx::util::unwrapped(&jacobi_kernel_mid);
    auto jacobi_bot =   hpx::util::unwrapped(&jacobi_kernel_bot);
    auto jacobi_top =   hpx::util::unwrapped(&jacobi_kernel_top);
    auto jacobi_left =  hpx::util::unwrapped(&jacobi_kernel_left);
    auto jacobi_right = hpx::util::unwrapped(&jacobi_kernel_right);
    auto jacobi_BL =    hpx::util::unwrapped(&jacobi_kernel_BL);
    auto jacobi_BR =    hpx::util::unwrapped(&jacobi_kernel_BR);
    auto jacobi_TL =    hpx::util::unwrapped(&jacobi_kernel_TL);
    auto jacobi_TR =    hpx::util::unwrapped(&jacobi_kernel_TR);

    void jacobi_init(vector< vector< vector< hpx::shared_future<block> > > > BlockList, size_t n, size_t block_size) {

        //allocate matrices
        //initialize block List
        size_t n_block = static_cast<size_t>(std::ceil(double(n)/block_size));
    }

    void jacobi( size_t n , size_t iterations, size_t block_size, std::string output_filename) {
        using hpx::lcos::local::dataflow;
        using hpx::lcos::shared_future;
        vector< vector< vector< shared_future<block> > > > blockList;
        jacobi_init(blockList, n, block_size);


        hpx::util::high_resolution_timer t;
        for(size_t i = 0; i < iterations; ++i) {
            //first column
            for(size_t j = 1; j < blockList[i%2].size() - 1; j++) {
                //first row
                for(size_t k = 1; k < blockList[i%2][j].size() - 1; k++) {
                    blockList[(i+1)%2][j][k] = dataflow( 
                            jacobi_op, blockList[i%2][j-1][k  ],
                                       blockList[i%2][j+1][k  ],
                                       blockList[i%2][j  ][k-1],
                                       blockList[i%2][j  ][k+1]);
                }
                //last row
            }
            //last column
        }
        hpx::wait_all(*deps_new);

        report_timing(n, iterations, t.elapsed());
        output_grid(output_filename, *grid_old, n);
   }
}
