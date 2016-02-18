all:
	g++ -Wall -Werror -std=c++11 -g ./src/mem.cpp -o ./bin/mem
	g++ -Wall -Werror -g ./src/memtricky.cpp -o ./bin/memtricky
	g++ -Wall -Werror -std=c++11 -g ./src/profsys.cpp -o ./bin/profsys
	g++ -Wall -Werror -std=c++11 -g ./src/profus.cpp -o ./bin/profus

clean:
	rm -rf bin/*

.PHONY: all clean
