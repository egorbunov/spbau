CFLAGS  := -m32 -Wall -g -fno-stack-protector -nostdinc

all: hello

hello: loader.o hello.o
	ld -melf_i386 -T hello.ld -nostdlib -o hello loader.o hello.o

loader.o: loader.S
	gcc -m32 -o loader.o -c loader.S

hello.o: hello.c
	gcc $(CFLAGS) -o hello.o -c hello.c

clean: 
	rm -f *.o *.bin hello hello1 *.elf
