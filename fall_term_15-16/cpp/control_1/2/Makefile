CPP_FLAGS=-std=c++11 -Werror -Wall
all: mergesort

mergesort: bin/mergesort.o bin/main.o bin/vector.o
	g++ ${CPP_FLAGS} bin/mergesort.o bin/main.o bin/vector.o -o bin/mergesort
bin/vector.o: src/vector.cpp src/vector.h
	g++ ${CPP_FLAGS} -c src/vector.cpp -o bin/vector.o
bin/mergesort.o: src/mergesort.cpp src/mergesort.h
	g++ ${CPP_FLAGS} -c src/mergesort.cpp -o bin/mergesort.o
bin/main.o: src/main.cpp src/mergesort.h src/vector.h
	g++ ${CPP_FLAGS} -c src/main.cpp -o bin/main.o 

clean:
	rm -f bin/mergesort bin/*.o bin/*.txt

.PHONY: clean all