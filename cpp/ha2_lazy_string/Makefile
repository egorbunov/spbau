CC = g++
LD = g++
CC_FLAGS = -Wall -Werror -std=c++11 -pedantic-errors

BIN_DIR = bin
SRC_DIR = src
BUILD_DIR = bin
TEST_DIR = test

SOURCES = $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS = $(patsubst $(SRC_DIR)/%.cpp, $(BIN_DIR)/%.o, $(SOURCES))

TEST_TARGET = tester.out
TEST_SOURCES = $(wildcard $(TEST_DIR)/*.cpp)
TEST_OBJECTS = $(patsubst $(TEST_DIR)/%.cpp, $(BIN_DIR)/%.o, $(TEST_SOURCES))
GTEST_LIBS = -lgtest_main -lgtest -lpthread
GTEST_LIB_PATH = /usr/src/googletest-release-1.7.0/build
GTEST_INCLUDE = /usr/src/googletest-release-1.7.0/include


DEBUG     ?= 0
ifeq ($(DEBUG), 1)
	CC_FLAGS += -O0 -g -pg -DDEBUG
else
	CC_FLAGS += -O3
endif


$(BUILD_DIR)/%.o: $(TEST_DIR)/%.cpp
	$(CC) -I$(GTEST_INCLUDE) -I$(SRC_DIR) $(CC_FLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CC) $(CC_FLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
	$(LD) $^ -o $(BIN_DIR)/$@

$(TEST_TARGET): $(TEST_OBJECTS) $(OBJECTS)
	$(LD) -I$(GTEST_INCLUDE) -L$(GTEST_LIB_PATH) $(GTEST_LIBS) $^ -o $(BIN_DIR)/$@

.PHONY: clean all test

all: $(TEST_TARGET)

test: $(TEST_TARGET)
	$(BIN_DIR)/$(TEST_TARGET) 

valgr: $(TEST_TARGET)
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all $(BIN_DIR)/$(TEST_TARGET) 

clean: 
	rm -rf $(BIN_DIR)/*