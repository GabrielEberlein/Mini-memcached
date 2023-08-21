common.o: common.c common.h log.h
log.o: log.c log.h
parser.o: parser.c parser.h common.h log.h
sock.o: sock.c sock.h common.h log.h
epoll.o : epoll.c epoll.h
stats.o : stats.c stats.h
node.o : node.c node.h
queue.o: queue.c queue.h node.h
bst.o: bst.c bst.h node.h queue.h
hash.o: stats.h hash.c hash.h node.h queue.h bst.h
