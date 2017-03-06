all: lint_test

lint_test: main.o lint.o
	g++ main.o lint.o -o lint_test

main.o: main.cpp
	g++ -std=c++11 -Wall -Werror -c main.cpp

lint.o: lint.cpp
	g++ -std=c++11 -Wall -Werror -c lint.cpp

clean:
	rm -rf *.o lint_test
