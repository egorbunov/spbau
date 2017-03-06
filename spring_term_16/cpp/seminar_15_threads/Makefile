BIN_DIR=./bin/
SRC_DIR=./src/

CPP_FLAGS=-Wall -Werror --std=c++11 -ggdb -pthread

$(BIN_DIR)/main: $(SRC_DIR)/main.cpp
	g++ $(CPP_FLAGS) $< -o $@

clean:
	rm -rf ./bin/*

.PHONY: clean
