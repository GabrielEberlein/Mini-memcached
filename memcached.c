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

#define MAX_EVENTS 10
#define MAX_THREADS 5

struct epoll_event events[MAX_EVENTS];
HashTable table;
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
	log(1,"NAZI\n");
    if(strcmp(cmd,"PUT") == 0){
		assert(nargs == 3);
        char *key = args[1];
        char *val = args[2];
        insert_hashtable(table, key, atoi(val));
		char reply[2024];
        sprintf(reply, "OK\n");
		write(fd, reply, strlen(reply));
    }

    if(strcmp(cmd,"GET") == 0){
		assert(nargs == 2);
        char *key = args[1];
        int res = search_hashtable(table, key);
		char reply[2024];
		if(res != -1)
        	sprintf(reply, "OK %d\n", res);
		else
			sprintf(reply, "ENOTFOUND");
		write(fd, reply, strlen(reply));
	}

    if(strcmp(cmd,"DEL") == 0){
		assert(nargs == 2);
        char *key = args[1];
        int res = delete_hashtable(table, key);
		char reply[2024];
        if(res != -1)
        	sprintf(reply, "OK\n");
		else
			sprintf(reply, "ENOTFOUND\n");
		write(fd, reply, strlen(reply));
    } 
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

		log(3, "Read %i bytes from fd %i", nread, fd);
		blen += nread;
		char *p, *p0 = buf;
		int nlen = blen;

		/* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Mensaje completo */
			int len = p - p0;
			*p++ = 0;
			log(3, "full command: <%s>", p0);
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(p0,toks,lens);

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

static int isnonblocking(int sfd)
{
	int flags, s;

	/* Obtiene las flags del socket */
	flags = fcntl (sfd, F_GETFL, 0);
	if (flags == -1) {
		perror ("fcntl");
		return -1;
	}

	/* 
	* Si la bandera O_NONBLOCK, la cual especifica que el socket no se bloquee,
	* no está en flags, la agrega
	*/
	flags |= O_NONBLOCK;
	s = fcntl (sfd, F_SETFL, flags);
	if (s == -1) {
		perror ("fcntl");
		return -1;
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
	printf("%d\n", limit);

    if (setrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("setrlimit");
        exit(EXIT_FAILURE);
    }
}

void handle_signals()
{
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
			if(events[i].data.fd == text_sock || events[i].data.fd == bin_sock) {
				log(1, "Nuevo Cliente\n");
				
				csock = accept(text_sock, NULL, NULL);
                if(csock == -1) {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }

                isnonblocking(csock);
								
				struct epoll_event ev;
				ev.data.u32 = (uint32_t)events[i].data.fd;
				ev.events = EPOLLIN | EPOLLET | EPOLLONESHOT;
                ev.data.fd = csock;

                if(epoll_ctl(efd, EPOLL_CTL_ADD, csock, &ev) == -1) {
                    perror("epoll_ctl: csock");
                    exit(EXIT_FAILURE);
                }

                ev.data.fd = text_sock;
				ev.events = EPOLLIN | EPOLLONESHOT;
				if (epoll_ctl(efd, EPOLL_CTL_MOD, text_sock, &ev) == -1) {
                	perror("epoll_ctl EPOLL_CTL_MOD");
                	exit(EXIT_FAILURE);
            	}
			} else {
				log(1, "Nuevo Mensaje\n");
				
				char buffer[2024];
				if (events[i].data.u32 == text_sock) {
					text_consume(buffer, events[i].data.fd, 0);
				} else {
					//bin_consume(buffer, events[i].data.fd, 0);
				}
				log(1, "Leyo el mensaje\n");

				events[i].events = EPOLLIN | EPOLLET | EPOLLONESHOT;
				if (epoll_ctl(efd, EPOLL_CTL_MOD, events[i].data.fd, &events[i]) == -1) {
                	perror("epoll_ctl EPOLL_CTL_MOD");
                	exit(EXIT_FAILURE);
            	}
			}
		}
	}
}

void server(int text_sock, int bin_sock) {
	
	int efd = epoll_create1(0);
	struct epoll_event ev;

	if(efd == -1) {
		perror("epoll_create1");
		exit(EXIT_FAILURE);
	}

	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.fd = text_sock;
	if (epoll_ctl (efd, EPOLL_CTL_ADD, text_sock, &ev) == -1) {
		perror("epoll_ctl: text_sock");
		exit(EXIT_FAILURE);
	}

	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.fd = bin_sock;
	if (epoll_ctl (efd, EPOLL_CTL_ADD, bin_sock, &ev) == -1) {
		perror("epoll_ctl: bin_sock");
		exit(EXIT_FAILURE);
	}

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

	/*En algún momento al manejar eventos de tipo EPOLLIN de un cliente 
	en modo texto invocaremos a text_consume: 
	int rc;
	rc = text_consume(evd, buff, fd, blen);
	y  al parecido habrá que hacer al momento al manejar eventos de tipo 
	EPOLLIN de un cliente en modo binario.	
	*/
}

int main(int argc, char **argv)
{
	
	int text_sock, bin_sock;

	__loglevel = 2;

	handle_signals();
	table = hashtable_create(1<<20);

	/*Función que limita la memoria*/
	//limit_mem(0);

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	// int commandsAmnt = 0;
	// char **commands = readfile(argv[1], &commandsAmnt);
	// /*Inicializar la tabla hash, con una dimensión apropiada*/
	// /* 1 millón de entradas, por ejemplo*/
	// table = hashtable_create(1<<20);

	server(text_sock, bin_sock);

	return 0;
}
