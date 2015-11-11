zCzC=gcc
LINKER_SCRIPT=src/lscript
LFLAGS=-T $(LINKER_SCRIPT) -m elf_i386 -nostdlib -nodefaultlibs
CFLAGS=-Wall -pedantic -MMD -I. -m32 -std=gnu99 -ffreestanding -g
OBJECTS=bin/loader.o bin/main.o bin/pic.o bin/util.o bin/pit.o bin/interrupt.o bin/threads.o bin/serial.o bin/spinlock.o bin/asm_switch.o
all: bin/threading

bin/threading: $(OBJECTS)
	ld $(LFLAGS) $^ -o $@ 

bin/%.o: src/%.S
	$(CC) -m32 -c $< -o $@   

bin/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@  

.PHONY: clean run

run: bin/threading
	(sleep 2; echo "Hello world! Hello world!! Hello world again!!!") | qemu-system-i386 -serial stdio -kernel $<

clean:
	rm -f bin/* bin/threading 