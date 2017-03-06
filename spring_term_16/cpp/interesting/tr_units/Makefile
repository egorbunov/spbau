CXX = g++
LD = g++
CFLAGS = -std=c++11 -O3

.PHONY: all clean

TARGET = main

all: $(TARGET)


$(TARGET): const.o global.o static.o main.o
	$(LD) $^ -o $@

const.o: const.cpp
	$(CXX) $(CFLAGS) -c $< -o $@

global.o: global.cpp
	$(CXX) $(CFLAGS) -c $< -o $@

static.o: static.cpp
	$(CXX) $(CFLAGS) -c $< -o $@

main.o: main.cpp
	$(CXX) $(CFLAGS) -c $< -o $@

clean:
	rm *.o
