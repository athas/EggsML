.PHONY: all clean

all: adventure

setup: hdr.h setup.c
	gcc -O3 -Wall -o setup setup.c

adventure: setup *.c *.h
	./setup glorkz > data.c
	gcc -Dlint -O3 -Wall -o adventure crc.c data.c done.c init.c io.c main.c save.c subr.c vocab.c wizard.c # No good reason to be smarter than this.

clean:
	rm -f setup data.c adventure
