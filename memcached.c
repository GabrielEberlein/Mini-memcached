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
#include "structures/hash.h"
#include "structures/queue.h"
#include "structures/stats.h"
#include "structures/node.h"
#include "commons/epoll.h"
#include "commons/stats.h"

#define MAX_EVENTS 10
#define MAX_THREADS 5

struct epoll_event events[MAX_EVENTS];
HashTable table;
Queue priorityqueue;

struct ThreadArgs{
	int text_sock;
	int bin_sock;
	int efd;
};

/* Macro interna */
#define READ(fd, buf, blen, n) ({							\
	int rc = read(fd, *buf + *blen, n);						\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))\
		return 0;											\
	if (rc <= 0){											\
		if((*buf) != NULL) { 								\
			free(*buf);										\									
			*buf = NULL; }									\
		*blen = 0;											\
		close(fd); 											\
		log(1, "Hola");										\
		return -1;}											\
	rc; }) 													\

void text_handle(int fd, char *args[3], int nargs){
	char *cmd = args[0];
	
    if(strcmp(cmd,"PUT") == 0) {
		stats_inc(table->stats, PUT_STAT);
		assert(nargs == 3);
		String key = string_create(args[1], strlen(args[1]));
		log(1, "Data: %s, Key: %d\n", key->data, key->len);
		String val = string_create(args[2], strlen(args[2]));
        hashtable_insert(priorityqueue, table, key, val);
		char reply[2024];
        sprintf(reply, "OK\n");
		write(fd, reply, 3);
    }

    if(strcmp(cmd,"GET") == 0) {
		stats_inc(table->stats, GET_STAT);
		assert(nargs == 2);
		String key = string_create(args[1], strlen(args[1]));
        String val = hashtable_search(priorityqueue, table, key);
		if(val != NULL){
        	write(fd, "OK ", 3);
			write(fd, val->data, val->len);
			write(fd,"\n",1);
		}
		else
			write(fd, "ENOTFOUND\n", 10);
	}

    if(strcmp(cmd,"DEL") == 0) {
		stats_inc(table->stats, DEL_STAT);
		assert(nargs == 2);
        String key = string_create(args[1], strlen(args[1]));
        int res = hashtable_delete(priorityqueue, table, key);
		char reply[2024];
        if(res != -1) {
			sprintf(reply, "OK\n");
		} else
			sprintf(reply, "ENOTFOUND\n");
		write(fd, reply, strlen(reply));
    } 

	if(strcmp(cmd,"STATS") == 0) {
		assert(nargs == 1);
		char reply[2024];
		int len = sprintf(reply, "OK PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu\n", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
		write(fd, reply, len);
    } 

	//Para printear la cola, sacarlo a la chucha cuando terminemos
	if(priorityqueue->first && priorityqueue->last){
		char* buf=malloc(2024);
		char* buf2=malloc(2024);
		Node node = priorityqueue->first;
		while(node){
			strncpy(buf2, node->key->data, node->key->len);
			buf = strcat(buf,buf2);
			node = node->prev;
		}
		log(1, "Queue: %s", buf);
	}
}

void bin_handle(int fd, char* args[3], int lens[3]){
	char cmd = args[0][0];
	switch (cmd)
	{
	case PUT:{
		stats_inc(table->stats, PUT_STAT);
		String key = string_create(args[1], lens[1]);
		log(1, "PUT - Data: %s, Len: %d\n", key->data, key->len);
		String val = string_create(args[2], lens[2]);
		log(1, "PUT - Data: %s, Len: %d\n", val->data, val->len);
        hashtable_insert(priorityqueue, table, key, val);
		char k = OK;
		write(fd, &k, 1);
		log(1,"Me mueroo\n");
		break;
	}
	case GET:{
		stats_inc(table->stats, GET_STAT);
		String key = string_create(args[1], lens[1]);
		log(1, "GET - Data: %s, Len: %d\n", key->data, key->len);
        String val = hashtable_search(priorityqueue, table, key);
		if(val != NULL){
			char k = OK;
        	write(fd, &k, 1);
			int len_net = htonl(val->len);
			write(fd, &len_net, 4);
			write(fd, val->data, val->len);
		}
		else{
			char enf = ENOTFOUND;
			write(fd, &enf, 1);
		}
		break;
	}
	case DEL:{
		stats_inc(table->stats, DEL_STAT);
        String key = string_create(args[1], lens[1]);
        int res = hashtable_delete(priorityqueue, table, key);
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
		char reply = OK, buffer[128];
		write(fd, &reply, 1);
		int len = sprintf(buffer, "PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
		int len_net = htonl(len);
		write(fd, &len_net, 4);
		write(fd, buffer, len);
	}
	default:
		break;
	}
}

int bin_consume(char** buf, int fd, int* blen) {
	log(1, "Entre, blen: %d", *blen);
	if(*buf==NULL) *buf = malloc(5);
	char *args[3]= {NULL};
	int lens[3] = {0};
	if ((*blen) == 0) (*blen) += READ(fd, buf, blen, 1);
	args[0] = (*buf);

	switch (buf[0][0]) {
		case PUT: {
			if ((*blen) < 1 + 4) (*blen) += READ(fd, buf, blen, 1 + 4 - (*blen));
			int len_net;
			memcpy(&len_net, (*buf) + 1, 4);
			log(1, "Network: %d", len_net);
			lens[1] = ntohl(len_net);
			log(1, "Host: %d", lens[1]);
			if((*blen)==5) (*buf) = realloc(*buf, 1+4+lens[1]+4);

			if ((*blen) < 1 + 4 + lens[1]) (*blen) += READ(fd, buf, blen, 1 + 4 + lens[1] - (*blen));
			args[1] = ((*buf) + 1 + 4);
			
			if ((*blen) < 1 + 4 + lens[1] + 4) (*blen) += READ(fd, buf, blen, 1 + 4 + lens[1] + 4 - (*blen));
			memcpy(&len_net, (*buf) + 1 + 4 + lens[1], 4);
			lens[2] = ntohl(len_net);
			if((*blen)==5 + lens[1] + 4) (*buf) = realloc(*buf, 1+4+lens[1]+4+lens[2]);
			(*blen) += READ(fd, buf, blen, 1 + 4 + lens[1] + 4 + lens[2] - (*blen));
			args[2] = ((*buf) + 1 + 4 + lens[1] + 4);

			bin_handle(fd,args,lens);
			break;
		}
		case GET: {
			if ((*blen) < 1 + 4) (*blen) += READ(fd, buf, blen, 1 + 4 - (*blen));
			int len_net;
			memcpy(&len_net, (*buf) + 1, 4);
			lens[1] = ntohl(len_net);
			if((*blen)==5) (*buf) = realloc(*buf, 1+4+lens[1]);

			(*blen) += READ(fd, buf, blen, 1 + 4 + lens[1] - (*blen));
			args[1] = ((*buf) + 1 + 4);

			bin_handle(fd,args,lens);
			break;
		}
		case DEL: {
			if ((*blen) < 1 + 4) (*blen) += READ(fd, buf, blen, 1 + 4 - (*blen));
			int len_net;
			memcpy(&len_net, (*buf) + 1, 4);
			lens[1] = ntohl(len_net);
			if((*blen)==5) (*buf) = realloc(*buf, 1+4+lens[1]);

			(*blen) += READ(fd, buf, blen, 1 + 4 + lens[1] - (*blen));
			args[1] = ((*buf) + 1 + 4);

			bin_handle(fd,args,lens);
			break;
		}
		case STATS: {
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

/* 0: todo ok, continua. -1 errores */
int text_consume(char** buf, int fd, int* blen) {
	if((*buf)==NULL) (*buf) = malloc(2048);
	while (1) {
		//int rem = sizeof *buf - blen;
		int rem = 2048 - (*blen);
		assert (rem >= 0);
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0)
			return -1;
		int nread = READ(fd, buf, blen, rem);	
		log(1, "Read %i bytes from fd %i", nread, fd);
		(*blen) += nread;
		char *p, *p0 = (*buf);
		int nlen = (*blen);

		/* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Mensaje completo */
			int len = p - p0;
			*p++ = 0;
			log(1, "full command: <%s>", p0);
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(p0, toks, lens);

			text_handle(fd, toks, ntok);
			/*
				Acá podríamos ver que hacemos con los tokens encontrados:
				toks[0] tendrá PUT, GET, DEL, ó STATS si se ingresó un comando válido.
			*/
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

void handle_signals() {
	/*Capturar y manejar  SIGPIPE */
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
				log(1, "Return: %d", r);
				if(r != -1) epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLET | EPOLLONESHOT);
			}
		}
	}
}

void server(int text_sock, int bin_sock) {
	int efd = epoll_init();

	epoll_add(efd, text_sock, TEXT, EPOLLIN | EPOLLONESHOT);
	epoll_add(efd, bin_sock, BIN, EPOLLIN | EPOLLONESHOT);

	struct ThreadArgs *args = (struct ThreadArgs*)malloc(sizeof(struct ThreadArgs));;
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

	handle_signals();
	table = hashtable_create(1<<20);
	priorityqueue = queue_create();

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	server(text_sock, bin_sock);

	return 0;
}
