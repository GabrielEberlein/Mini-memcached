CC = gcc
CFLAGS += -Wall -O3 -g
LDFLAGS += -pthread

all: memcached 

memcached: memcached.o server.o commons/sock.o commons/common.o commons/log.o commons/parser.o commons/epoll.o structures/stats.o structures/structures.o

clean:
	rm -f memcached *.o commons/*.o structures/*.o *.out *.beam *.dump 


run: all
	./memcached

.deps.mk:
	$(CC) -MM *.c > .deps.mk

.PHONY: all clean run

include .deps.mk
