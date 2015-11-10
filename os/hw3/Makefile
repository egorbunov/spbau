zCzC=gcc
LINKER_SCRIPT=src/lscript
LFLAGS=-nostdlib -m elf_i386 -T $(LINKER_SCRIPT)
CFLAGS=-ffreestanding -fno-builtin -O -ggdb -std=c99 -m32
OBJECTS=bin/loader.o bin/main.o bin/pic.o bin/util.o bin/pit.o bin/interrupt.o bin/threads.o bin/serial.o bin/spinlock.o
all: bin/threading

bin/threading: $(OBJECTS) $(LINKER_SCRIPT)
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