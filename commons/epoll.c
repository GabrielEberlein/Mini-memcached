#include "epoll.h"
#include "../structures/structures.h"

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
	Data* data = safe_malloc(sizeof(Data));
    data->fd = sock;
    data->mode = mode;
	data->buf = NULL;
	data->blen = 0;
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

void epoll_del(int efd, Data* data){
	if((data->buf) != NULL) {
		free(data->buf);												
		data->buf = NULL;
	}		
	data->blen = 0;		
	epoll_ctl(efd, EPOLL_CTL_DEL, data->fd, NULL);							
	close(data->fd);
	free(data);
}