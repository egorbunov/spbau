all: ./bin/test

CPP_FLAGS=-Wall -Werror -ggdb --static
LIBS=-lpthread

./bin/%: ./src/%.cpp
	g++ $(CPP_FLAGS) $< $(LIBS)  -o $@

clean:
	rm -f ./bin/*

.PHONY: clean all
