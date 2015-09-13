# Название любого таргета соответствует имени результирующего файла (пути к файлу)
# Справа от таргета идут команда (посе :), которые должны выполниться для достижения цели
# Ключик для мэйкфайла -j для распределение компиляции по ядрам. Описание порядка сборки --
# это дерево, поэтому алгоритм сборки легко распараллеливается.

CPP_FLAGS=-Werror -Wall
all: main

main: bin/add.o bin/main.o
	g++ ${CPP_FLAGS} bin/add.o bin/main.o -o bin/main
bin/add.o: src/add.cpp
	g++ ${CPP_FLAGS} -c src/add.cpp -o bin/add.o
bin/main.o: src/main.cpp
	g++ ${CPP_FLAGS} -c src/main.cpp -o bin/main.o 

clean:
	rm -f bin/main bin/*.o

.PHONY: clean all