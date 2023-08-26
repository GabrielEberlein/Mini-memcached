common.o: common.c common.h log.h
log.o: log.c log.h
parser.o: parser.c parser.h common.h log.h
sock.o: sock.c sock.h common.h log.h
epoll.o: epoll.c epoll.h
stats.o: stats.c stats.h
structures.o: structures.c structures.h stats.h