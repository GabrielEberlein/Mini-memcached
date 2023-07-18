common.o: common.c common.h log.h
log.o: log.c log.h
parser.o: parser.c parser.h common.h log.h
sock.o: sock.c sock.h common.h log.h
epoll.o: epoll.c epoll.h
hash.o: hash.c hash.h bst.h
bst.o: bst.c bst.h