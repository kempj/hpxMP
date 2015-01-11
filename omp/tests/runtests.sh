#!/bin/bash
RT=$1
CC=$2
TEST_DIR=bin/$RT/$CC
logfile=logs/$CC-$RT.log

preload=../../$RT

echo "running the $CC compiled code with the $RT runtime"
echo "output written to $logfile"

echo `date` > $logfile

for test in $TEST_DIR/*
do
    echo "running $test" | tee -a $logfile
    LD_PRELOAD=$preload timeout 30s ./$test >>$logfile 2>&1
    retval=$?
    if [ $retval -eq 124 ]
    then 
        echo "ERROR: test ran for longer than 30 seconds" | tee -a $logfile
    elif [ $retval -ne 0 ]
    then echo "Error: test returned ($retval)" | tee -a $logfile
    fi

done

