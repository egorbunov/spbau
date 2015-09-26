all: hello

hello: hello.elf
	objcopy -O binary hello.elf hello

hello.o: hello.c
	gcc -Werror -Wall -ffreestanding -c -Os hello.c -o hello.o

hello.elf: hello.o
	ld -static -Thello.ld -nostdlib --nmagic -o hello.elf hello.o

.PHONY: clean all

clean:
	rm -f hello *.elf *.o


