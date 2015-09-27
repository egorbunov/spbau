CFLAGS  := -m32 -Wall -nostdinc
all: hello

hello: hello.elf
	objcopy -O binary hello.elf hello

hello.o: hello.c
	gcc ${CFLAGS} -c hello.c -o hello.o

hello.elf: hello.o
	ld -m elf_i386 -T hello.ld -nostdlib -o hello.elf hello.o

.PHONY: clean all

clean:
	rm -f hello *.elf *.o