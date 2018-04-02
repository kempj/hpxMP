//  Copyright (c) 2007-2017 Hartmut Kaiser, Richard D Guidry Jr.
//  Copyright (c) 2011 Vinay C Amatya
//  Copyright (c) 2011 Bryce Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
//  Parts of this nqueen_client.cpp has been taken from the accumulator example
//  by Hartmut Kaiser.


//#include <boost/lexical_cast.hpp>

#include <cstddef>
#include <iostream>
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


int main(int argc, char* argv[]) 
{
    const int default_size = 8;

    int soln_count_total = 0;

    std::cout << "Enter size of board. Default size is 8." << std::endl;
    std::cout << "Command Options: size[value] | default | print | quit"
              << std::endl;
    std::string cmd;
    std::cin >> cmd;
#pragma omp parallel
{
#pragma omp single
{
    while (std::cin.good()) {
        int sz = default_size;
        if(cmd == "size") {
            soln_count_total = 0;
            std::string arg;
            std::cin >> arg;
            sz = atoi(arg.c_str());
        }

        nqueen::board *sub_boards = new nqueen::board[sz];
        int *sub_count = new int[sz];
        auto t1 = high_resolution_clock::now();
        for(int i=0; i < sz; i++) {
            sub_boards[i] = nqueen::board();
            sub_boards[i].init_board(sz);
            #pragma omp task 
            {
                sub_count[i] = task_create( sub_boards[i], sz, i);
            }
        }
        #pragma omp taskwait
        for(int i=0; i < sz; i++) {
            soln_count_total += sub_count[i];
        }
        auto t2 = high_resolution_clock::now();

        auto total = duration_cast<nanoseconds> (t2-t1).count();
        std::cout << "time: " << total << " ns" << std::endl;
        std::cout << "soln_count:" << soln_count_total << std::endl;
        delete[] sub_boards;
        delete[] sub_count;

        if(cmd == "print") {
            std::cout << "soln_count : " << soln_count_total << std::endl;
        } else if (cmd == "quit") {
            break;
        } else {
            std::cout << "Options: size[value] | default | print "<<
            "| quit" << std::endl;
        }
        std::cin >> cmd;
    }
}}//end parallel
    return 0;
}

