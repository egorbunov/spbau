DEFINES=-DCONFIG_DEBUG=1 -DCONFIG_LOG_LEVEL=0

bin/test: src/main.cpp src/myassert.h src/mylog.h src/instructions.h
	g++ -Wall -Werror -ggdb $(DEFINES) -std=c++11 $< -o $@

clean:
	rm -rf bin/*

.PHONY: clean
