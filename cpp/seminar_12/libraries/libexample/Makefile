CPP_FLAGS=-std=c++11

all: bin/testa bin/testso

bin/testa: bin/test.o bin/libexample.a
	g++ $^ -o $@

bin/testso: bin/test.o bin/libexample.so
	g++ $< -o $@ -Lbin -lexample

bin/test.o: src/test.cpp
	g++ $(CPP_FLAGS) -c $< -o $@

bin/libexample.so: bin/libexample.o
	g++ -shared $< -o $@

bin/libexample.a: bin/libexample.o
	ar rcs $@ $<

bin/libexample.o: src/libexample.cpp src/libexample.h
	g++ $(CPP_FLAGS) -c $< -fPIC -o $@

.PHONE: all clean

clean:
	rm -rf bin/*
