CC = clang
CFLAGS += -Wall -O3
LDFLAGS += -pthread

all: memcached 


memcached: memcached.o commons/sock.o commons/common.o commons/log.o commons/parser.o commons/epoll.o hashtable/bst.o hashtable/hash.o

clean:
	rm -f memcached *.o commons/*.o hashtable/*.o


run: all
	./memcached

.deps.mk:
	$(CC) -MM *.c > .deps.mk

.PHONY: all clean run

include .deps.mk
