/****************************************************************************
 *                                                                           *
 *             OpenMP MicroBenchmark Suite - Version 3.0                     *
 *                                                                           *
 *                            produced by                                    *
 *                                                                           *
 *             Mark Bull, Fiona Reid and Nix Mc Donnell                      *
 *                                                                           *
 *                                at                                         *
 *                                                                           *
 *                Edinburgh Parallel Computing Centre                        *
 *                                                                           *
 *         email: markb@epcc.ed.ac.uk or fiona@epcc.ed.ac.uk                 *
 *                                                                           *
 *                                                                           *
 *      This version copyright (c) The University of Edinburgh, 2011.        *
 *                                                                           *
 *                                                                           *
 *  Licensed under the Apache License, Version 2.0 (the "License");          *
 *  you may not use this file except in compliance with the License.         *
 *  You may obtain a copy of the License at                                  *
 *                                                                           *
 *      http://www.apache.org/licenses/LICENSE-2.0                           *
 *                                                                           *
 *  Unless required by applicable law or agreed to in writing, software      *
 *  distributed under the License is distributed on an "AS IS" BASIS,        *
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
 *  See the License for the specific language governing permissions and      *
 *  limitations under the License.                                           *
 *                                                                           *
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#include "common.h"
#include "taskbench.h"


#define DEPTH 6

void parse_args( int argc, char *argv[], int &delay_reps, int &inner_reps, int &outer_reps) {
    for(int arg = 1; arg < argc; arg++) {
        if( string(argv[arg]) == "--outer-repetitions" ) {
            outer_reps = atoi(argv[++arg]);
        } else if( string(argv[arg]) ==  "--inner-repetitions" ) {
            inner_reps=atoi(argv[++arg]);
        } else if( string(argv[arg]) == "--delay-repetitions" ) {
            delay_reps=atoi(argv[++arg]);
        } else {
            printf("parameters are:\n");
            printf("\t--delay-repetitions: the number of repetitions for the delay function\n");
            printf("\t--inner-repetitions: the number of of tasks created in each test.\n");
            printf("\t--outer-repetitions: the number of times to run each test\n");
            exit(0);
        }
    }
    if(outer_reps == 0 || inner_reps < 1 || delay_reps < 1) {
        printf("Invalid input: number of repetitions must be > 0");
    }
}
int main(int argc, char **argv) {
    int delay_reps = 1000, inner_reps = 512, outer_reps = 20; 
    parse_args(argc, argv, delay_reps, inner_reps, outer_reps);

    printf("Running OpenMP benchmark version 3.0\n"
            "\t%d thread(s)\n"
            "\t%d outer repetitions\n"
            "\t%d inner repetitions\n"
            "\t%d delay repetitions\n",
            omp_get_max_threads(), outer_reps, inner_reps, delay_reps);
    printf("\n--------------------------------------------------------\n");

    benchmark("PARALLEL TASK", &testParallelTaskGeneration, delay_reps, inner_reps, outer_reps);
    benchmark("MASTER TASK", &testMasterTaskGeneration, delay_reps, inner_reps, outer_reps);
    benchmark("MASTER TASK BUSY SLAVES", &testMasterTaskGenerationWithBusySlaves, delay_reps, inner_reps, outer_reps);
    benchmark("TASK WAIT", &testTaskWait, delay_reps, inner_reps, outer_reps);
    benchmark("TASK BARRIER", &testTaskBarrier, delay_reps, inner_reps, outer_reps);
    benchmark("NESTED TASK", &testNestedTaskGeneration, delay_reps, inner_reps, outer_reps);
    benchmark("NESTED MASTER TASK", &testNestedMasterTaskGeneration, delay_reps, inner_reps, outer_reps);
    benchmark("BRANCH TASK TREE", &testBranchTaskGeneration, delay_reps, inner_reps, outer_reps);
    benchmark("LEAF TASK TREE", &testLeafTaskGeneration, delay_reps, inner_reps, outer_reps);

    return 0;
}

void testParallelTaskGeneration(int inner_reps, int delay_reps) {
    int j;
#pragma omp parallel private( j )
    {
        for ( j = 0; j < inner_reps; j ++ ) {
#pragma omp task
            {
                delay( delay_reps );
            } 
        }
    }
}

void testMasterTaskGeneration(int inner_reps, int delay_reps) {
    int j;
    int nthreads = omp_get_num_threads();
#pragma omp parallel private(j)
    {
#pragma omp master
        {
            // Since this is executed by one thread we need inner_reps * nthreads iterations 
            for (j = 0; j < inner_reps * nthreads; j++) {
#pragma omp task
                {
                    delay(delay_reps);
                }
            } 
        }
    } 
}

void testMasterTaskGenerationWithBusySlaves(int inner_reps, int delay_reps) {
    int j;
#pragma omp parallel private( j )
    {
        int thread_num = omp_get_thread_num();
        for (j = 0; j < inner_reps; j ++ ) {
            if ( thread_num == 0 ) {
#pragma omp task
                {
                    delay( delay_reps );
                } 
            } else {
                delay( delay_reps );
            }
        }
    }
}

void testNestedTaskGeneration(int inner_reps, int delay_reps) {
    int i,j;
    int nthreads = omp_get_num_threads();
#pragma omp parallel private( i, j )
    {
        for ( j = 0; j < inner_reps / nthreads; j ++ ) {
#pragma omp task private( i )
            {
                for ( i = 0; i < nthreads; i ++ ) {
#pragma omp task untied
                    {
                        delay( delay_reps );
                    }
                }
#pragma omp taskwait
            }
        }
    }
}

void testNestedMasterTaskGeneration(int inner_reps, int delay_reps) {
    int i, j;
    int nthreads = omp_get_num_threads();
#pragma omp parallel private( i, j )
    {
#pragma omp master
        {
            for ( j = 0; j < inner_reps; j ++ ) {
#pragma omp task private( i )
                {
                    for ( i = 0; i < nthreads; i ++ ) {
#pragma omp task
                        {
                            delay( delay_reps );
                        }
                    }
#pragma omp taskwait
                }
            }
        }
    }
}

void testTaskWait(int inner_reps, int delay_reps) {
    int j;
#pragma omp parallel private( j )
    {
        for ( j = 0; j < inner_reps; j ++ ) {
#pragma omp task
            {
                delay( delay_reps );
            }
#pragma omp taskwait
        }
    }
}

void testTaskBarrier(int inner_reps, int delay_reps) {
    int j;
#pragma omp parallel private( j )
    {
        for ( j = 0; j < inner_reps; j ++ ) {
#pragma omp task
            {
                delay( delay_reps );
            }
#pragma omp barrier
        }
    }
}

void testBranchTaskGeneration(int inner_reps, int delay_reps) {
    int j;
#pragma omp parallel private(j)
    {
        for (j = 0; j < (inner_reps >> DEPTH); j++) {
#pragma omp task
            {
                branchTaskTree(DEPTH, delay_reps);
                delay(delay_reps);
            }
        }
    }
}

void branchTaskTree(int tree_level, int delay_reps) {
    if ( tree_level > 0 ) {
#pragma omp task
        {
            branchTaskTree(tree_level - 1, delay_reps);
            branchTaskTree(tree_level - 1, delay_reps);
            delay(delay_reps);
        }
    }
}

void testLeafTaskGeneration(int inner_reps, int delay_reps) {
    int j;
#pragma omp parallel private(j)
    {
        for (j = 0; j < (inner_reps >> DEPTH); j++) {
            leafTaskTree(DEPTH, delay_reps);
        }
    }
}

void leafTaskTree(int tree_level, int delay_reps) {
    if ( tree_level == 0 ) {
        delay(delay_reps);
    } else {
#pragma omp task
        {
            leafTaskTree(tree_level - 1, delay_reps);
            leafTaskTree(tree_level - 1, delay_reps);
        }
    }
}

