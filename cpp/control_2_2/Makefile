BIN_DIR=bin/
TEST_BIN=$(BIN_DIR)/test

CFLAGS=-Wall -Werror -std=c++11

all: $(TEST_BIN)

$(TEST_BIN): src/test.cpp \
	src/reflection.h \
	src/binary_serialization.h \
	src/json_serialization.h
	g++ $(CFLAGS) $< -o $@

clean:
	rm -rf bin/*

.PHONY: clean
