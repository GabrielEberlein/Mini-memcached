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
#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))	\
		return 0;						\
	if (rc <= 0){							\
		close(fd);						\
		return -1;}						\
	rc; })

void text_handle(int fd, char *args[3], int nargs){
	char *cmd = args[0];
	
    if(strcmp(cmd,"PUT") == 0) {
		stats_inc(table->stats, 0);
		assert(nargs == 3);
		String key = string_create(args[1], strlen(args[1]));
		log(1, "Data: %s, Key: %d\n", key->data, key->len);
		String val = string_create(args[2], strlen(args[2]));
        hashtable_insert(priorityqueue, table, key, val);
		char reply[2024];
        sprintf(reply, "OK\n");
		write(fd, reply, strlen(reply));
    }

    if(strcmp(cmd,"GET") == 0) {
		stats_inc(table->stats, 1);
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
		stats_inc(table->stats, 2);
		assert(nargs == 2);
        String key = string_create(args[1], strlen(args[1]));
        int res = hashtable_delete(priorityqueue, table, key);
		char reply[2024];
        if(res != -1) {
			sprintf(reply, "OK\n");
			stats_dec(table->stats, 2);
		} else
			sprintf(reply, "ENOTFOUND\n");
		write(fd, reply, strlen(reply));
    } 

	if(strcmp(cmd,"STATS") == 0) {
		assert(nargs == 2);
		char reply[2024];
		int len = snprintf(reply, 0, "PUTS=%d GETS=%d DELS=%d KEYS=%d", table->stats[0], table->stats[1], table->stats[2], table->stats[3]);
		write(fd, reply, len);
    } 

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

int bin_consume(int fd, int blen){
	char cmd;
	int nread = READ(fd, &cmd, 1);
	switch (cmd) {
	case PUT: {
		stats_inc(table->stats, 0);
		int len_net;
		nread = READ(fd, &len_net, 4);
		int keyLen = ntohl(len_net);
		char* keyData = malloc(keyLen);
		nread = READ(fd, keyData, keyLen);
		String key = string_create(keyData, nread);
		log(1, "Data: %s, Key: %d\n", keyData, keyLen);
		free(keyData);

		nread = READ(fd, &len_net, 4);
		int valLen = ntohl(len_net);
		char* valData = malloc(valLen);
		nread = READ(fd, valData, valLen);
		String val = string_create(valData, nread);
		free(valData);

		hashtable_insert(priorityqueue, table, key, val);
		stats_inc(table->stats, 3);

		char k = OK;
		write(fd, &k, 1);
		break;
	}
	case GET: {
		log(1, "GETTEANDO\n");
		stats_inc(table->stats, 1);
		int len_net;
		nread = READ(fd, &len_net, 4);
		int keyLen = ntohl(len_net);
		char* keyData = malloc(keyLen);
		nread = READ(fd, keyData, keyLen);
		String key = string_create(keyData, keyLen);
		free(keyData);
		log(1,"Key: %s, len: %d",key->data, key->len);

		String value = hashtable_search(priorityqueue, table, key);

		if(value != NULL) {
			char k = OK;
			len_net = htonl(value->len);
			write(fd, &k, 1);
			write(fd, &len_net, 4);
			write(fd, value->data, value->len);
		} else {
			char enotfound = ENOTFOUND;
			write(fd, &enotfound, 1);
		}

		break;
	}
	case DEL: {
		stats_inc(table->stats, 2);
		char lbuf[4];
		nread = READ(fd, lbuf, 4);
		int len_net = *(int*)lbuf;
		int keyLen = ntohl(len_net);
		char* keyData = malloc(keyLen);
		nread = READ(fd, keyData, keyLen);
		String key = string_create(keyData, keyLen);
		free(keyData);

		int res = hashtable_delete(priorityqueue, table, key);

		char reply;
		if (res != -1) {
			reply = OK;
			stats_dec(table->stats, 2);
		} else reply = ENOTFOUND;

		write(fd, &reply, 1);

		break;
	}

	case STATS:	{
		char reply = OK, buffer[2024];
		write(fd, &reply, 1);
		int len = snprintf(buffer, 0, "PUTS=%d GETS=%d DELS=%d KEYS=%d", table->stats[0], table->stats[1], table->stats[2], table->stats[3]);
		int len_net = htonl(len);
		write(fd, &len_net, 4);
		write(fd, buffer, len);
	}

	default:
		break;
	}
	return 1;
}

/* 0: todo ok, continua. -1 errores */
int text_consume(char buf[2024], int fd, int blen)
{
	while (1) {
		//int rem = sizeof *buf - blen;
		int rem = 2024 - blen;
		assert (rem >= 0);
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0)
			return -1;
		int nread = READ(fd, buf + blen, rem);	
		log(1, "Read %i bytes from fd %i", nread, fd);
		blen += nread;
		char *p, *p0 = buf;
		int nlen = blen;

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
		if (p0 != buf) {
			memmove(buf, p0, nlen);
			blen = nlen;
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
				char buffer[2024];
				int r;
				if(data->mode == TEXT) r = text_consume(buffer, data->fd, 0);
				if(data->mode == BIN) r = bin_consume(data->fd, 0);
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
