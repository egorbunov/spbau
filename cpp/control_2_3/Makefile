BIN_DIR=bin/
TEST_BIN=$(BIN_DIR)/test

CFLAGS=-Wall -Werror -std=c++11

all: $(TEST_BIN)

$(TEST_BIN): src/test.cpp src/au_allocator.h
	g++-4.9 $(CFLAGS) $< -o $@

clean:
	rm -rf bin/*

.PHONY: clean
