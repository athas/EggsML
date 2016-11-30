.PHONY: all clean

all: adventure

adventure: bsdgames-2.17/adventure/*.h bsdgames-2.17/adventure/*.c
	cd bsdgames-2.17 && ./configure
	cd bsdgames-2.17 && make
	cp bsdgames-2.17/adventure/adventure .

clean:
	rm -f adventure
	cd bsdgames-2.17 && make clean
