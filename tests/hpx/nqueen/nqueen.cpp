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
#include <numeric>

#include "nqueen.hpp"


using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::nanoseconds;


//int task_create( nqueen::board sub_board, int size, int i) 
//{
//    return sub_board.solve_board( sub_board.access_board(), size, 0, i);
//}

/*
{
    board b(list, size, level);

    if (level == size) {
        return 1;
    } else if (level == 0) {
        b.update_board(level, col);
        if (b.check_board(b.access_board(), level)) {
            b.count_ += solve_board(b.access_board(), size, level + 1, col);
        }
    } else {
        for (int i = 0; i < size; ++i) {
            b.update_board(level, i);
            if (b.check_board(b.access_board(), level)) {
                b.count_ += solve_board(b.access_board(), size, level + 1, col);
            }
        }
    }
    return b.count_;
}
*/
int sum_count_futures(std::vector<hpx::shared_future<int> > counts)
{
    int sum = 0;
    for(int i=0; i<counts.size(); i++) {
        sum += counts[i].get();
    }
    return sum;
}

hpx::shared_future<int> task_create( nqueen::board sub_board, int size, int col, int task_level)
{
    if (task_level == 0) {
        return hpx::async(&nqueen::board::solve_board, sub_board, sub_board.access_board(), size, 0, col);
    } 

    nqueen::board b(sub_board.access_board(), size, 0);
    std::vector<hpx::shared_future<int> > sub_count(size);

    for(int i=0; i<size; i++) {
        b.update_board(0, i);
        if (b.check_board(b.access_board(), 0)) {
            //b.count_ += solve_board(b.access_board(), size, 1, i);
            sub_count[i] = hpx::async(&nqueen::board::solve_board, b, b.access_board(), size, 1, i);
        }
    }

    hpx::shared_future<std::vector<hpx::shared_future<int> > > count_futures = hpx::when_all(sub_count);

    return hpx::dataflow( hpx::util::unwrapping(sum_count_futures), count_futures);
}


int hpx_main(int argc, char* argv[])
{
    int soln_count_total = 0;
    int sz = 8;
    int task_level = 1;
    if(argc > 1) {
        sz = atoi(argv[1]);
    } else if(argc > 2) {
        task_level = atoi(argv[2]);
    }


    std::cout << "board size: " << sz << std::endl;

    std::vector<nqueen::board> sub_boards;
    std::vector<hpx::shared_future<int> > sub_count(sz);
    auto t1 = high_resolution_clock::now();
    for(int i=0; i < sz; i++) {
        sub_boards.push_back(nqueen::board());
        sub_boards[i].init_board(sz);
        sub_count[i] = task_create(sub_boards[i], sz, i, task_level);
        //sub_count[i] = hpx::async(&task_create, sub_boards[i], sz, i);
        //sub_count[i] = hpx::async(&nqueen::board::solve_board, sub_boards[i], sub_boards[i].access_board(), sz , 0, i);
    }
    for(int i=0; i < sz; i++) {
        soln_count_total += sub_count[i].get();
    }
    auto t2 = high_resolution_clock::now();

    auto total = duration_cast<nanoseconds> (t2-t1).count();
    std::cout << "time: " << total << " ns" << std::endl;
    std::cout << "soln_count: " << soln_count_total << std::endl;
    sub_boards.clear();

    return hpx::finalize();
}

int main(int argc, char* argv[])
{
    return hpx::init(argc, argv);
}
