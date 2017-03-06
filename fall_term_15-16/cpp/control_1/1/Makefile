CPP_FLAGS=-Werror -Wall
all: mergesort

mergesort: bin/mergesort.o bin/main.o
	g++ ${CPP_FLAGS} bin/mergesort.o bin/main.o -o bin/mergesort
bin/mergesort.o: src/mergesort.cpp src/mergesort.h
	g++ ${CPP_FLAGS} -c src/mergesort.cpp -o bin/mergesort.o
bin/main.o: src/main.cpp src/mergesort.h
	g++ ${CPP_FLAGS} -c src/main.cpp -o bin/main.o 

clean:
	rm -f bin/mergesort bin/*.o

.PHONY: clean all