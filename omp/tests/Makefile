CC = clang
RT = libiomp5.so

CFLAGS = -g -fopenmp

SRCS = $(wildcard src/*.c)

TESTS = $(patsubst src/%.c,bin/$(RT)/$(CC)/%,$(SRCS))

default: $(TESTS)

bin/$(RT)/$(CC)/%: src/%.c
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -rf bin/$(RT)/$(CC)/*
