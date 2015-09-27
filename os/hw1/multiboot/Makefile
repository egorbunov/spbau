all: hello

hello: hello.ld hello.o loader.o printf.o screen.o
	ld -T hello.ld -o hello hello.o loader.o screen.o printf.o

screen.o: screen.c screen.h
	gcc -Iinclude -Wall -fno-builtin -nostdinc -ffreestanding -nostdlib -o screen.o -c screen.c

printf.o: printf.c printf.h
	gcc -Iinclude -Wall -fno-builtin -nostdinc -ffreestanding -nostdlib -o printf.o -c printf.c

hello.o: hello.c
	gcc -Iinclude -Wall -fno-builtin -nostdinc -ffreestanding -nostdlib -o hello.o -c hello.c

loader.o: loader.S
	as loader.S -o loader.o

clean: 
	rm -f *.o hello *.out

.PHONY: clean all