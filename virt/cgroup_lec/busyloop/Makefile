all: ./bin/test ./bin/run.sh

CPP_FLAGS=-Wall -Werror -ggdb --static
LIBS=-lpthread

./bin/%: ./src/%.cpp
	g++ $(CPP_FLAGS) $< $(LIBS)  -o $@

./bin/run.sh: ./src/run.sh
	cp $< $@

clean:
	rm -f ./bin/*

.PHONY: clean all
