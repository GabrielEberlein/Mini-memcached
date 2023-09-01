#include "server.h"

int terminate_threads = 0;

void bin_handle(int fd, char* args[3], int lens[3]){
	char cmd = args[0][0];
	//log(1, "bin_handle args:%d", args[0][0]);
	//log(1, "cmd: %d", cmd);
	switch (cmd) {
		case PUT: {
			stats_inc(table->stats, PUT_STAT);
			hashtable_insert(args[1], lens[1], args[2], lens[2], 1);
			char k = OK;
			// log(1, "PUT %s %s", args[1], args[2]);
			write(fd, &k, 1);
			break;
		}
		case GET:{
			// log(1,"GET: %s", args[1]);
			stats_inc(table->stats, GET_STAT);
			int bin;
			String val = hashtable_search(args[1], lens[1], &bin);
			if(val != NULL) {
				char k = OK;
				write(fd, &k, 1);
				int len_net = htonl(val->len);
				write(fd, &len_net, 4);
				write(fd, val->data, val->len);
				string_destroy(val);
			} else {
				char enf = ENOTFOUND;
				write(fd, &enf, 1);
			}
			break;
		}
		case DEL:{
			// log(1, "DEL: %s", args[1]);
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
			// log(1, "STATS");
			char reply = OK, buffer[128];
			write(fd, &reply, 1);
			int len = sprintf(buffer, "PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
			int len_net = htonl(len);
			write(fd, &len_net, 4);
			write(fd, buffer, len);
			break;
		}
		default: {
			//log(1, "bin_handle DEFAULT");
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

	// log(1, "blen: %d, port: %d",*blen, fd);
	// log(1, "buf: %d, port: %d", buf[0][0], fd);
	char cmd = buf[0][0];
	switch (cmd) {
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
			//if(buf[0][0] == GET) log(1, "Me confundi");
			//log(1, "bin_consume DEFAULT");
			char reply = EINVALID;
			write(fd, &reply, 1);
			break;
		}
	}
	if((*buf) != NULL) {
		free(*buf);											
		*buf = NULL;
	}
	*blen = 0;
	//log(1, "Se completo la task port:%d", fd);
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
		//log(1, "Inserto, Data: %s, Largo: %d, Data: %s, Largo: %d", args[1], lens[1], args[2], lens[2]);
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
		//log(1, "Busco, Data: %s, Largo: %d", args[1], lens[1]);
		int bin=0;
        String val = hashtable_search(args[1], lens[1], &bin);
		if(val != NULL){
			if(bin == 1) {
				write(fd, "EBINARY\n", 8);
			} else{
				if(val->len > 2045) {
					write(fd, "EBIG\n", 5);
				} else {
					write(fd, "OK ", 3);
					write(fd, val->data, val->len);
					write(fd,"\n",1);
				}
			}
            string_destroy(val);
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
		(*blen) += READ(fd, *buf, *blen, rem);	
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

void *thread(void *args) {
	
	int nfds, csock;
	struct epoll_event events[MAX_EVENTS];

	struct ThreadArgs* thread_args = (struct ThreadArgs*)args;
    int mock_event = thread_args->mock_event;
    int text_sock = thread_args->text_sock;
	int bin_sock = thread_args->bin_sock;
    int efd = thread_args->efd;

	while(!terminate_threads) {
		nfds = epoll_wait(efd, events, MAX_EVENTS, -1);
		if(nfds == -1) {
			perror("epoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i < nfds; i++) {
			Data* data = events[i].data.ptr;
			if(data->fd == mock_event) {
				uint64_t dummy;
				read(mock_event, &dummy, sizeof(uint64_t));
				write(mock_event, &dummy, sizeof(uint64_t));
			}else if(data->fd == text_sock || data->fd == bin_sock) {
				csock = new_client(data->fd);
				log(1, "Nuevo Cliente id:%d\n", csock);
				epoll_add(efd, csock, data->mode, EPOLLIN | EPOLLET | EPOLLONESHOT);
				epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLET | EPOLLONESHOT);
			} else {
				int r;
				if(data->mode == TEXT) r = text_consume(&(data->buf), data->fd, &(data->blen));
				if(data->mode == BIN) r = bin_consume(&(data->buf), data->fd, &(data->blen));
				
				if(r == 0) epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLET | EPOLLONESHOT);
				else{
					log(1, "Cierro Cliente id:%d\n", data->fd);
					epoll_del(efd, data);							
				}
			}
		}
	}
}

void server(int text_sock, int bin_sock, int mock_event) {
	int efd = epoll_init();
	epoll_add(efd, mock_event, TEXT, EPOLLIN | EPOLLET);
	epoll_add(efd, text_sock, TEXT, EPOLLIN | EPOLLET | EPOLLONESHOT);
	epoll_add(efd, bin_sock, BIN, EPOLLIN | EPOLLET | EPOLLONESHOT);

	struct ThreadArgs *args = (struct ThreadArgs*)safe_malloc(sizeof(struct ThreadArgs));
    args->mock_event = mock_event;
	args->text_sock = text_sock;
	args->bin_sock = bin_sock;
	args->efd = efd;

	long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    log(1,"Server running with %d threads.",num_cores);
	pthread_t threads[num_cores];
    for (int i = 0; i < num_cores; i++) {
        if (pthread_create(&threads[i], NULL, thread, args) != 0) {
            fprintf(stderr, "Error creando el hilo %d\n", i);
            exit(EXIT_FAILURE);
        }
    }

	for (int i = 0; i < num_cores; i++) {
        if (pthread_join(threads[i], NULL) != 0) {
            fprintf(stderr, "Error al esperar al hilo %d\n", i);
            exit(EXIT_FAILURE);
        }
    }

	free(args);
	close(efd);
}
