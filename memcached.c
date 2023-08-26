#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <fcntl.h>
#include "commons/sock.h"
#include "commons/common.h"
#include "commons/parser.h"
#include "structures/stats.h"
#include "structures/structures.h"
#include "commons/epoll.h"

#define MAX_EVENTS 10
#define MAX_THREADS 5

struct epoll_event events[MAX_EVENTS];

struct ThreadArgs{
	int text_sock;
	int bin_sock;
	int efd;
};

/* Macro interna */
#define READ(fd, buf, blen, n) ({							\
	int rc = read(fd, buf + blen, n);						\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))\
		return 0;											\
	if (rc <= 0)											\
		return -1;											\
	rc; }) 													\

#define READ_BINARG(fd, buf, blen, off, len) ({ 												\
	if(blen==off) buf = safe_realloc(buf, off + 4);											\
	if (blen < off + 4) blen += READ(fd, buf, blen, off + 4 - blen);						\
	int len_net;																					\
	memcpy(&len_net, buf + off, 4);																\
	len = ntohl(len_net);																			\
	if(blen==off + 4) buf = safe_realloc(buf, off + 4 + len);								\
	if (blen < off + 4 + len) blen += READ(fd, buf, blen, off + 4 + len - blen);			\
})																									\

void bin_handle(int fd, char* args[3], int lens[3]){
	char cmd = args[0][0];
	log(1, "bin_handle args:%d", args[0][0]);
	log(1, "cmd: %d", cmd);
	switch (cmd) {
		case PUT: {
			stats_inc(table->stats, PUT_STAT);
			hashtable_insert(args[1], lens[1], args[2], lens[2], 1);
			char k = OK;
			log(1, "PUT %c", k);
			write(fd, &k, 1);
			break;
		}
		case GET:{
			stats_inc(table->stats, GET_STAT);
			Node node = hashtable_search(args[1], lens[1]);
			if(node != NULL) {
				String val = node->val;
				log(1,"Gettie: %s, %d", val->data, val->len);
				char k = OK;
				write(fd, &k, 1);
				int len_net = htonl(val->len);
				write(fd, &len_net, 4);
				write(fd, val->data, val->len);
			} else {
				char enf = ENOTFOUND;
				log(1,"NOTFOUND :(");
				write(fd, &enf, 1);
			}
			break;
		}
		case DEL:{
			log(1, "DEL");
			stats_inc(table->stats, DEL_STAT);
			int res = hashtable_delete(args[1], lens[1]);
			if(res != -1){
				char k = OK;
				write(fd, &k, 1);
			}
			else{
				char enf = ENOTFOUND;
				write(fd, &enf, 1);
			}
			break;
		}
		case STATS:	{
			log(1, "STATS");
			char reply = OK, buffer[128];
			write(fd, &reply, 1);
			int len = sprintf(buffer, "PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
			int len_net = htonl(len);
			write(fd, &len_net, 4);
			write(fd, buffer, len);
			break;
		}
		default: {
			log(1, "DEFAULT");
			char reply = EINVALID;
			write(fd, &reply, 1);
			break;
		}
	}
}

int bin_consume(char** buf, int fd, int* blen) {
	if(*buf==NULL) *buf = safe_malloc(1);
	char *args[3]= {NULL};
	int lens[3] = {0};
	if ((*blen) == 0) (*blen) += READ(fd, *buf, *blen, 1);

	log(1, "buf: %d", buf[0][0]);
	switch (buf[0][0]) {
		case PUT: {
			READ_BINARG(fd, *buf, *blen, 1, lens[1]);
			READ_BINARG(fd, *buf, *blen, 1 + 4 + lens[1], lens[2]);
			args[0]=*buf;
			args[1]=*buf+1+4;
			args[2]=*buf+1+4+lens[1]+4;
			bin_handle(fd,args,lens);
			break;
		}
		case GET: {
			READ_BINARG(fd, *buf, *blen, 1, lens[1]);
			args[0] = *buf;
			args[1] = *buf+1+4;
			bin_handle(fd, args, lens);
			break;
		}
		case DEL: {
			READ_BINARG(fd, *buf, *blen, 1, lens[1]);
			args[0] = *buf;
			args[1] = *buf+1+4;
			bin_handle(fd, args, lens);
			break;
		}
		case STATS: {
			args[0] = *buf;
			bin_handle(fd,args,lens);
			break;
		}
		default: {
			break;
		}
	}
	if((*buf) != NULL) {
		free(*buf);											
		*buf = NULL;
	}
	*blen = 0;
	return 0;
}

void text_handle(int fd, char *args[3], int lens[3], int nargs){
	char *cmd = args[0];
	
    if(strcmp(cmd,"PUT") == 0) {
		if(nargs != 3) {
			write(fd, "EINVALID\n", 9);
			return;
		}
		stats_inc(table->stats, PUT_STAT);
		log(1, "Inserto, Data: %s, Largo: %d, Data: %s, Largo: %d", args[1], lens[1], args[2], lens[2]);
        hashtable_insert(args[1], lens[1], args[2], lens[2], 0);
		char reply[2024];
        sprintf(reply, "OK\n");
		write(fd, reply, 3);
    } 
	else if(strcmp(cmd,"GET") == 0) {
		if(nargs != 2) {
			write(fd, "EINVALID\n", 9);
			return;
		}
		stats_inc(table->stats, GET_STAT);
		log(1, "Busco, Data: %s, Largo: %d", args[1], lens[1]);
        Node node = hashtable_search(args[1], lens[1]);
		if(node != NULL){
			if(node->binary == 1) {
				write(fd, "EBINARY\n", 8);
			} else{
				String val = node->val;
				if(val->len > 2045) {
					write(fd, "EBIG\n", 5);
				} else {
					write(fd, "OK ", 3);
					write(fd, val->data, val->len);
					write(fd,"\n",1);
				}
			}
		}
		else write(fd, "ENOTFOUND\n", 10);
	} 
	else if(strcmp(cmd,"DEL") == 0) {
		if(nargs != 2) {
			write(fd, "EINVALID\n", 9);
			return;
		}
		stats_inc(table->stats, DEL_STAT);
        int res = hashtable_delete(args[1], lens[1]);
		char reply[2024];
        if(res != -1) {
			sprintf(reply, "OK\n");
		} else
			sprintf(reply, "ENOTFOUND\n");
		write(fd, reply, strlen(reply));
    } 
	else if(strcmp(cmd,"STATS") == 0) {
		if(nargs != 1) {
			char reply[10];
        	sprintf(reply, "EINVALID\n");
			write(fd, reply, 10);
			return;
		}
		char reply[2024];
		int len = sprintf(reply, "OK PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu\n", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
		write(fd, reply, len);
    } 
	else {
		char reply[10];
		sprintf(reply, "EINVALID\n");
		write(fd, reply, 10);
	}

	//Para printear la cola, sacarlo a la chucha cuando terminemos
	// if(queue->first && queue->last){
	// 	char* buf=safe_malloc(2024);
	// 	char* buf2=safe_malloc(2024);
	// 	Node node = queue->first;
	// 	while(node){
	// 		strncpy(buf2, node->key->data, node->key->len);
	// 		buf = strcat(buf,buf2);
	// 		node = node->prev;
	// 	}
	// 	log(1, "Queue: %s", buf);
	// }
}

/* 0: todo ok, continua. -1 errores */
int text_consume(char** buf, int fd, int* blen) {
	if((*buf)==NULL) (*buf) = safe_malloc(2048);
	while (1) {
		//int rem = sizeof *buf - blen;
		int rem = 2048 - (*blen);
		assert (rem >= 0);
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0) {
			write(fd, "EINVALID\n", 7);
			return -1;
		}
		(*blen) += READ(fd, buf, *blen, rem);	
		char *p, *p0 = (*buf);
		int nlen = (*blen);

		/* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Mensaje completo */
			int len = p - p0;
			*p++ = 0;
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(p0, toks, lens);

			text_handle(fd, toks, lens, ntok);

			nlen -= len + 1;
			p0 = p;
		}

		/* Si consumimos algo, mover */
		if (p0 != (*buf)) {
			memmove((*buf), p0, nlen);
			(*blen) = nlen;
		}
	}
	return 0;
}

void limit_mem(size_t limit)
{
	struct rlimit mem_limit;

    if (getrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("getrlimit");
        exit(EXIT_FAILURE);
    }

    // mem_limit.rlim_cur = 1073741824;  // 1 GB
	mem_limit.rlim_max = limit;
	mem_limit.rlim_cur = limit;
	printf("%zu\n", limit);

    if (setrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("setrlimit");
        exit(EXIT_FAILURE);
    }
}

void handle_signals(int s) {
	log(1, "SIGPIPE ERROR");
}

void *thread(void *args) {
	
	int nfds, csock;

	struct ThreadArgs* thread_args = (struct ThreadArgs*)args;

    int text_sock = thread_args->text_sock;
	int bin_sock = thread_args->bin_sock;
    int efd = thread_args->efd;

	while(1) {
		nfds = epoll_wait(efd, events, MAX_EVENTS, -1);
		if(nfds == -1) {
			perror("epoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i < nfds; i++) {
			Data* data = events[i].data.ptr;
			
			if(data->fd == text_sock || data->fd == bin_sock) {
				csock = new_client(data->fd);
				log(1, "Nuevo Cliente id:%d\n", csock);
				epoll_add(efd, csock, data->mode, EPOLLIN | EPOLLET | EPOLLONESHOT);
				epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLONESHOT);
			} else {
				int r;
				if(data->mode == TEXT) r = text_consume(&(data->buf), data->fd, &(data->blen));
				if(data->mode == BIN) r = bin_consume(&(data->buf), data->fd, &(data->blen));
				log(1, "r:%d", r);
				if(r != -1) epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLET | EPOLLONESHOT);
				else{
					if((data->buf) != NULL) {
						free(data->buf);												
						data->buf = NULL;
					}		
					data->blen = 0;									
					close(data->fd); 									
				}
			}
		}
	}
}

void server(int text_sock, int bin_sock) {
	int efd = epoll_init();

	epoll_add(efd, text_sock, TEXT, EPOLLIN | EPOLLONESHOT);
	epoll_add(efd, bin_sock, BIN, EPOLLIN | EPOLLONESHOT);

	struct ThreadArgs *args = (struct ThreadArgs*)safe_malloc(sizeof(struct ThreadArgs));
	args->text_sock = text_sock;
	args->bin_sock = bin_sock;
	args->efd = efd;

	pthread_t threads[MAX_THREADS];
    for (int i = 0; i < MAX_THREADS; ++i) {
        if (pthread_create(&threads[i], NULL, thread, args) != 0) {
            fprintf(stderr, "Error creando el hilo %d\n", i);
            exit(EXIT_FAILURE);
        }
    }

	for (int i = 0; i < MAX_THREADS; ++i) {
        if (pthread_join(threads[i], NULL) != 0) {
            fprintf(stderr, "Error al esperar al hilo %d\n", i);
            exit(EXIT_FAILURE);
        }
    }

}

int main(int argc, char **argv)
{
	int text_sock, bin_sock;

	__loglevel = 2;
	//Magic number: 11400000 5 puts
	limit_mem(1111400000);
	signal(SIGPIPE, handle_signals);
	hashtable_create(1);
	queue_create();

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	server(text_sock, bin_sock);

	return 0;
}
