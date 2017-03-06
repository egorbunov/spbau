BIN_NAME := prsum
BIN_DIR := ./bin
CXX := g++
CXXFLAGS += -Wall -Werror -Wextra -std=c++11 -pedantic-errors -pthread
LDFLAGS += -lrt -lOpenCL
SRC_DIR := .

SOURCES = $(shell find $(SRC_DIR) -name '*.cpp' -printf '%T@\t%p\n'\ | sort -k 1nr | cut -f2-)
OBJECTS = $(SOURCES:$(SRC_DIR)/%.cpp=$(BIN_DIR)/%.o)

.PHONY: all
all: $(BIN_DIR) $(BIN_DIR)/$(BIN_NAME)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

.PHONY: clean
clean:
	rm -rf $(BIN_DIR)/*

$(BIN_DIR)/$(BIN_NAME): $(OBJECTS)
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $@

$(BIN_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

