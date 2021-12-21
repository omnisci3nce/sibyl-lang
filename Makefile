output.exe: output.o
	gcc -no-pie -nostartfiles lib/c/addTen.c output.o  -o output.exe

output.o: output.s
	nasm -f elf64 output.s -o output.o

output.s: lib/*
	dune exec bin/main.exe

test:
	cd tests && ./test.sh

clean:
	rm *.o *.s output