#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <assert.h>
#include <pthread.h>
#include <errno.h>
#include "commons/sock.h"
#include "commons/common.h"
#include "commons/parser.h"
#include "structures/stats.h"
#include "structures/structures.h"
#include "commons/epoll.h"

#define MAX_EVENTS 10

extern int terminate_threads;
struct ThreadArgs{
    int mock_event;
	int text_sock;
	int bin_sock;
	int efd;
};

/* Macro interna */
#define READ(fd, buf, blen, n) ({							\
	int pblen = blen; 										\
	int pn = n;												\
	void* pbuf = buf;										\
	int rc = read(fd, buf + blen, n);						\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))\
		return 0;											\
	if (rc <= 0)											\
		return -1;											\
	rc; }) 													\

#define READ_BINARG(fd, buf, blen, off, len) ({ 											\
	if(blen==off) buf = safe_realloc(buf, off + 4);											\
	if (blen < off + 4) blen += READ(fd, buf, blen, off + 4 - blen);						\
	int len_net;																			\
	memcpy(&len_net, buf + off, 4);															\
	len = ntohl(len_net);																	\
	if(blen==off + 4) buf = safe_realloc(buf, off + 4 + len);								\
	if (blen < off + 4 + len) blen += READ(fd, buf, blen, off + 4 + len - blen);			\
})																							\

void server(int text_sock, int bin_sock, int mock_event);