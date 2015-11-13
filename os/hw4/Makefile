zCzC=gcc
LINKER_SCRIPT=src/lscript
LFLAGS=-T $(LINKER_SCRIPT) -m elf_i386 -nostdlib -nodefaultlibs
CFLAGS=-Wall -pedantic -MMD -I. -m32 -std=gnu99 -ffreestanding -g
OBJECTS=bin/loader.o bin/main.o bin/util.o bin/serial.o bin/vga.o bin/string.o bin/multiboot.o
all: bin/mbinfo

bin/mbinfo: $(OBJECTS)
	ld $(LFLAGS) $^ -o $@ 

bin/%.o: src/%.S
	$(CC) -m32 -c $< -o $@   

bin/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@  

.PHONY: clean run

run: bin/mbinfo
	qemu-system-i386 -kernel $< -append "param1,,param2"

clean:
	rm -f bin/* bin/mbinfo 