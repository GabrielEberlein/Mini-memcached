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
#include "hashtable/hash.h"
#include "queue/queue.h"
#include "commons/epoll.h"
#include "commons/stats.h"

#define MAX_EVENTS 10
#define MAX_THREADS 5

struct epoll_event events[MAX_EVENTS];
HashTable table;
Stats stats;
Queue* priorityqueue;

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
	if (rc <= 0)							\
		return -1;						\
	rc; })

void text_handle(int fd, char *args[3], int nargs){
	char *cmd = args[0];
	
    if(strcmp(cmd, "PUT") == 0) {
		increase(stats, PUTS);

		assert(nargs == 3);
        char *key = args[1];
        char *val = args[2];

        insert_hashtable(table, key, val, strlen(key), strlen(val));
		increase(stats, KEYS);

		char reply[2024];
        sprintf(reply, "OK\n");
		write(fd, reply, 3);
    }

    if(strcmp(cmd, "GET") == 0) {
		increase(stats, GETS);

		assert(nargs == 2);
        char *key = args[1];

        char* value = search_hashtable(table, key, strlen(key));

		char reply[2024];
		if(value != NULL)
        	sprintf(reply, "OK %s\n", value);
		else
			sprintf(reply, "ENOTFOUND\n");
		write(fd, reply, strlen(reply));
	}

    if(strcmp(cmd, "DEL") == 0){
		increase(stats, DELS);

		assert(nargs == 2);
        char *key = args[1];

        int res = delete_hashtable(table, key, strlen(key));

		char reply[2024];
        if(res != -1) {
			decrease(stats, KEYS);
        	sprintf(reply, "OK\n");
		} else 
			sprintf(reply, "ENOTFOUND\n");
		write(fd, reply, strlen(reply));
    }

	if(strcmp(cmd, "STATS") == 0) {
		char reply[2024];
		sprintf(reply, "OK PUTS: %d GETS: %d DELS: %d KEYS:%d\n", stats->counters[PUTS],
																  stats->counters[GETS],
																  stats->counters[DELS],
																  stats->counters[KEYS]);
		write(fd, reply, strlen(reply));
	}
}

int bin_consume(int fd, int blen){
	char cmd;
	int nread = READ(fd, &cmd, 1);
	switch (cmd) {
	case PUT: {
		increase(stats, PUTS);

		char lbuf[4];
		nread = READ(fd, lbuf, 4);
		int len_net = *(int*)lbuf;
		int keyLen = ntohl(len_net);
		char* key = malloc(keyLen);
		nread = READ(fd, key, keyLen);

		nread = READ(fd, lbuf, 4);
		len_net = *(int*)lbuf;
		int valLen = ntohl(len_net);
		char* val = malloc(valLen);
		nread = READ(fd, val, valLen);

		insert_hashtable(table, key, val, keyLen, valLen);
		increase(stats, KEYS);

		free(key);
		free(val);

		char k = OK;
		write(fd, &k, 1);

		break;
	}
	case GET: {
		increase(stats, GETS);

		char lbuf[4];
		nread = READ(fd, lbuf, 4);
		int len_net = *(int*)lbuf;
		int keyLen = ntohl(len_net);
		char* key = malloc(keyLen);
		nread = READ(fd, key, keyLen);

		char* value = search_hashtable(table, key, keyLen);

		free(key);

		if(value != NULL) {
			char k = OK;
			int valLen = strlen(value);
			len_net = htonl(valLen);
			write(fd, &k, 1);
			write(fd, &len_net, 4);
			write(fd, value, valLen);
		} else {
			char enotfound = ENOTFOUND;
			write(fd, &enotfound, 1);
		}

		break;
	}
	case DEL: {
		increase(stats, DELS);

		char lbuf[4];
		nread = READ(fd, lbuf, 4);
		int len_net = *(int*)lbuf;
		int lenKey = ntohl(len_net);
		char* key = malloc(lenKey);
		nread = READ(fd, key, lenKey);

		int res = delete_hashtable(table, key, lenKey);

		free(key);

		if (res != -1) {
			decrease(stats, KEYS);
			char k = OK;
			write(fd, &k, 1);
		} else {
			char enotfound = ENOTFOUND;
			write(fd, &enotfound, 1);
		}

		break;
	}
	case STATS:	{
		char k = OK;
		write(fd, &k, 1);
		char reply[2024];
		sprintf(reply, "OK PUTS: %d GETS: %d DELS: %d KEYS:%d\n", stats->counters[PUTS],
																  stats->counters[GETS],
																  stats->counters[DELS],
																  stats->counters[KEYS]);
		write(fd, reply, strlen(reply));
		break;
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
				log(1, "Nuevo Cliente\n");
				csock = new_client(data->fd);
				epoll_add(efd, csock, data->mode, EPOLLIN | EPOLLET | EPOLLONESHOT);
				epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLONESHOT);
			} else {
				char buffer[2024];
				if(data->mode == TEXT) text_consume(buffer, data->fd, 0);
				if(data->mode == BIN) bin_consume(data->fd, 0);
				epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLET | EPOLLONESHOT);
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
	stats = stats_create();

	/*Función que limita la memoria*/
	//limit_mem(0);

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	server(text_sock, bin_sock);

	return 0;
}
