# Название любого таргета соответствует имени результирующего файла (пути к файлу)
# Справа от таргета идут команда (посе :), которые должны выполниться для достижения цели
# Ключик для мэйкфайла -j для распределение компиляции по ядрам. Описание порядка сборки --
# это дерево, поэтому алгоритм сборки легко распараллеливается.

CPP_FLAGS=-Werror -Wall
all: main

main: bin/a.o bin/main.o bin/add.o
	g++ ${CPP_FLAGS} bin/a.o bin/main.o bin/add.o -o bin/main
bin/a.o: src/a.cpp src/a.h
	g++ ${CPP_FLAGS} -c src/a.cpp -o bin/a.o
bin/add.o: src/add.cpp src/add.h
	g++ ${CPP_FLAGS} -c src/add.cpp -o bin/add.o
bin/main.o: src/main.cpp src/a.h src/add.h
	g++ ${CPP_FLAGS} -c src/main.cpp -o bin/main.o 

clean:
	rm -f bin/main bin/*.o

.PHONY: clean all