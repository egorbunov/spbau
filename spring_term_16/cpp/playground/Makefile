TARGET = main

CC = g++
LD = g++
CC_FLAGS = -Wall -Werror -std=c++11 -pedantic-errors

BIN_DIR = bin
SRC_DIR = src
BUILD_DIR = bin

SOURCES = $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS = $(patsubst $(SRC_DIR)/%.cpp, $(BIN_DIR)/%.o, $(SOURCES))

DEBUG     ?= 0
ifeq ($(DEBUG), 1)
	CFLAGS += -O0 -g -pg -DDEBUG
	LDFLAGS = -O0 -g -pg -DDEBUG
else
	CFLAGS += -O3
	LDFLAGS = -O3
endif

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CC) $(CC_FLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
	$(LD) $^ -o $(BIN_DIR)/$@

.PHONY: clean all

all: $(TARGET)

clean: 
	rm -rf $(BIN_DIR)/* 