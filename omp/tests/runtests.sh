#!/bin/bash
RT=$1
CC=$2
TEST_DIR=bin/$RT/$CC
logfile=logs/$CC-$RT.log

echo "running the $CC compiled code with the $RT runtime"
echo $logfile

for test in $TEST_DIR/*
do
    echo "running $test"
    timeout 30s ./$test >> $logfile
    if [ $? -eq 124 ]
    then 
        echo "ERROR: test ran for longer than 30 seconds"
    fi
done

