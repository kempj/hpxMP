CC = clang
BIN_DIR = bin/clang

CFLAGS = -g -fopenmp

SRCS = $(wildcard src/*.c)

TESTS= $(patsubst src/%.c,$(BIN_DIR)/%,$(SRCS))

default: $(TESTS)


$(BIN_DIR)/%: src/%.c
	$(CC) $(CFLAGS) -o $@ $<


.PHONY: clean
clean:
	rm -rf $(BIN_DIR)/*
