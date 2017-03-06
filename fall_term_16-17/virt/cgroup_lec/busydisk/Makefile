all: bin/ bin/test

bin/:
	mkdir bin

./bin/%: ./src/%.cpp
	g++ $(CPP_FLAGS) $< $(LIBS)  -o $@

clean:
	rm -f ./bin/*

.PHONY: clean all