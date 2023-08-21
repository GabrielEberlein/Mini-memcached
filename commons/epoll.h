#include <stdio.h>
#include <stdlib.h>
#include <sys/epoll.h>

enum modes {TEXT, BIN};

typedef struct Data {
    int fd;
    enum modes mode;
    char* buf;
    int blen;
} Data;

int epoll_init();

void epoll_add(int efd, int sock, enum modes mode, int events);

void epoll_mod(int efd, int sock, enum modes mode, Data* data, int events);