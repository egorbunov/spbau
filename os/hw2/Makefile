CC=gcc
LINKER_SCRIPT=src/lscript
LFLAGS=-nostdlib -m elf_i386 -T $(LINKER_SCRIPT)
CFLAGS=-ffreestanding -fno-builtin -O -ggdb -std=c99 -m32
OBJECTS=bin/loader.o bin/main.o bin/pic.o bin/util.o bin/pit.o bin/interrupt.o
all: bin/baretick

bin/baretick: $(OBJECTS) $(LINKER_SCRIPT)
	ld $(LFLAGS) $^ -o $@ 

bin/%.o: src/%.s
	$(CC) -m32 -c $< -o $@   

bin/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@  

.PHONY: clean run

run: bin/baretick
	qemu-system-i386 -kernel $<

clean:
	rm -f bin/* bin/baretick 