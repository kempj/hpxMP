//  Copyright (c) 2007-2017 Hartmut Kaiser, Richard D Guidry Jr.
//  Copyright (c) 2011 Vinay C Amatya
//  Copyright (c) 2011 Bryce Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
//  Parts of this nqueen_client.cpp has been taken from the accumulator example
//  by Hartmut Kaiser.

#include <hpx/hpx.hpp>
#include <hpx/hpx_init.hpp>
#include <hpx/include/lcos.hpp>

#include <boost/lexical_cast.hpp>

#include <cstddef>
#include <iostream>
#include <list>
#include <string>
#include <vector>
#include <chrono>


#include "nqueen.hpp"


using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::nanoseconds;


int task_create( nqueen::board sub_board, int size, int i)
{
    return sub_board.solve_board( sub_board.access_board(), size, 0, i);
}


int hpx_main(int argc, char* argv[])
{
    int soln_count_total = 0;
    int sz = 8;
    if(argc > 1) {
        sz = atoi(argv[1]);
    }


    std::cout << "board size: " << sz << std::endl;

    std::vector<nqueen::board> sub_boards;
    std::vector<hpx::shared_future<int> > sub_count(sz);
    auto t1 = high_resolution_clock::now();
    for(int i=0; i < sz; i++) {
        sub_boards.push_back(nqueen::board());
        sub_boards[i].init_board(sz);
        sub_count[i] = hpx::async(&task_create, sub_boards[i], sz, i);
    }
    for(int i=0; i < sz; i++) {
        soln_count_total += sub_count[i].get();
    }
    auto t2 = high_resolution_clock::now();

    auto total = duration_cast<nanoseconds> (t2-t1).count();
    std::cout << "time: " << total << " ns" << std::endl;

    std::cout << "soln_count: " << soln_count_total << std::endl;
    sub_boards.clear();

    hpx::finalize();

    return 0;
}

int main(int argc, char* argv[])
{
    return hpx::init(argc, argv);
}
