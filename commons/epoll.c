#include "epoll.h"

int epoll_init(){
    int efd = epoll_create1(0);

	if(efd == -1) {
		perror("epoll_create1");
		exit(EXIT_FAILURE);
	}
    return efd;
}

void epoll_add(int efd, int sock, enum modes mode, int events){
    struct epoll_event ev;
    ev.events = events;
	Data* data = malloc(sizeof(Data));
    data->fd = sock;
    data->mode = mode;
	ev.data.ptr = data;
	if (epoll_ctl (efd, EPOLL_CTL_ADD, sock, &ev) == -1) {
		perror("epoll_ctl: add");
		exit(EXIT_FAILURE);
	}
}

void epoll_mod(int efd, int sock, enum modes mode, Data* data, int events){
    struct epoll_event ev;
    ev.events = events;
	data->fd = sock;
    data->mode = mode;
    ev.data.ptr = data;
	if (epoll_ctl (efd, EPOLL_CTL_MOD, sock, &ev) == -1) {
		perror("epoll_ctl: mod");
		exit(EXIT_FAILURE);
	}
}