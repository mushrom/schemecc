all: prog

test.s: foo.scm
	gojira foo.scm > test.s

stub.o: stub.c

scm.o: test.s
	nasm -f elf64 -o scm.o test.s

prog: stub.o scm.o
	gcc -o $@ *.o

clean:
	rm *.o
	rm test.s
	rm prog

test: prog
	./prog
