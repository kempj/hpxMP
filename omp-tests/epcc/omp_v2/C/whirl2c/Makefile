#omni compiler
#CC = omcc
#CFLAGS =
#LDFLAGS =

#intel compiler 
#CC = icc 
#CFLAGS = -openmp
#LDFLAGS = -openmp -lm

# orcc2.1.uh
#CC = orcc 
#CFLAGS = -mp -lm 
#LDFLAGS = -mp -lm

# whirl2c last step
CC = gcc
CFLAGS = -g 
LDFLAGS = -lpthread -lm 
C_INC = -I /home/l/liao/usr/bin/include

CPP = /usr/bin/cpp
# Uncomment the following line to use OMP2.0 features
#OMPFLAG = -DOMPVER2
CLOCKFLAG = -DOMPCLOCK
#CLOCKFLAG = -DGTODCLOCK
LIBS = $(HOME)/orc2.1.uh/src/osprey1.0/libopenmp/libomp.a

.c.o:
	${CC} ${CFLAGS}  -c $*.c 

syncbench.o:
	${CC} ${CFLAGS} ${C_INC} -o syncbench.o -c syncbench.w2c.c 
schedbench.o:
	${CC} ${CFLAGS} ${C_INC} -o schedbench.o -c schedbench.w2c.c 

SYNCOBJS =  syncbench.o delay.o getclock.o 
SCHEDOBJS = schedbench.o delay.o getclock.o 
#ARRAYOBJS = arraybench_$(IDA).o arraydelay.o getclock.o 
CLOCKOBJS = gtod.o 

all:
#	$(MAKE) IDA=1 prog
#	$(MAKE) IDA=3 prog
#	$(MAKE) IDA=9 prog
#	$(MAKE) IDA=27 prog
#	$(MAKE) IDA=81 prog
#	$(MAKE) IDA=243 prog
#	$(MAKE) IDA=729 prog
#	$(MAKE) IDA=2187 prog
#	$(MAKE) IDA=6561 prog
#	$(MAKE) IDA=19683 prog
#	$(MAKE) IDA=59049 prog

prog: arraybench_$(IDA) syncbench schedbench

syncbench: $(SYNCOBJS) $(CLOCKOBJS)
	$(CC) -o syncbench $(LDFLAGS) $(SYNCOBJS) $(CLOCKOBJS) $(LIBS) -lm 

schedbench: $(SCHEDOBJS) $(CLOCKOBJS)
	$(CC) -o schedbench  $(LDFLAGS) $(SCHEDOBJS) $(CLOCKOBJS) $(LIBS) -lm 

getclock.o: $(CLOCKOBJS) getclock.c
	$(CC) $(CFLAGS) $(CLOCKFLAG) getclock.c -c -o $@

# Multiple header files due to multiple array sizes, makes header file arraybench_*.h
arraybench_$(IDA).h: arraybench.h
	$(CPP) -DIDA=$(IDA) $(OMPFLAG) -P arraybench.h -o $@

# Multiple object files due to multiple array sizes, makes object file arraybench_*.o
arraybench_$(IDA).o: arraybench_$(IDA).h arraybench.c
	$(CC) $(CFLAGS) -DIDA=$(IDA) $(OMPFLAG) arraybench.c -c -o $@

# Multiple executables due to multiple array sizes, makes exe file arraybench_*
arraybench_$(IDA): $(ARRAYOBJS) $(CLOCKOBJS) arraybench.c
	$(CC) $(LDFLAGS) $(ARRAYOBJS) $(CLOCKOBJS) $(LIBS) -lm -o $@ 

clean: 
	rm *.o syncbench schedbench arraybench_*


