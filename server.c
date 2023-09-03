#include "server.h"

// Bandera que indica si es necesario terminar los threads
int terminate_threads = 0;

// bin_handle : int, char**, int -> int
/*
	Maneja las instrucciones del protocolo binario
*/
int bin_handle(int fd, char* args[3], int lens[3]){
	char cmd = args[0][0];
	switch (cmd) {
		// Instrucción PUT
		case PUT: {
			// Realizamos las acciones necesarias
			stats_inc(table->stats, PUT_STAT);
			hashtable_insert(args[1], lens[1], args[2], lens[2], 1);
			// Devolvemos el resultado
			char k = OK;
			if (writen(fd, &k, 1) == -1) return -1;
			break;
		}
		// Instrucción GET
		case GET: {
			// Realizamos las acciones necesarias
			stats_inc(table->stats, GET_STAT);
			int bin;
			String val = hashtable_search(args[1], lens[1], &bin);
			// Devolvemos el resultado
			if(val != NULL) {
				char k = OK;
				if (writen(fd, &k, 1) == -1) return -1;
				int len_net = htonl(val->len);
				if (writen(fd, &len_net, 4) == -1) return -1;
				if (writen(fd, val->data, val->len) == -1) return -1;
				string_destroy(val);
			} else {
				char enf = ENOTFOUND;
				if (writen(fd, &enf, 1) == -1);
			}
			break;
		}
		// Instrucción DEL
		case DEL:{
			// Realizamos las acciones necesarias
			stats_inc(table->stats, DEL_STAT);
			int res = hashtable_delete(args[1], lens[1]);
			// Devolvemos el resultado
			if(res != -1){
				char k = OK;
				if (writen(fd, &k, 1) == -1) return -1;
			}
			else{
				char enf = ENOTFOUND;
				if (writen(fd, &enf, 1) == -1) return -1;
			}
			break;
		}
		// Instrucción STATS
		case STATS:	{
			// Devolvemos el resultado
			char reply = OK, buffer[128];
			if (writen(fd, &reply, 1) == -1) return -1;
			int len = sprintf(buffer, "PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
			int len_net = htonl(len);
			if (writen(fd, &len_net, 4) == -1) return -1;
			if (writen(fd, buffer, len) == -1) return -1;
			break;
		}
		// La instrucción es incorrecta
		default: {
			char reply = EINVALID;
			if (writen(fd, &reply, 1) == -1) return -1;
			break;
		}
	}
	return 0;
}

// bin_consume : char**, int, int* -> int
/*
	Va acumulando en un buffer hasta donde puede leer de un socket
	Al leer lograr leer todos los caracteres especificados, entiende que el
	comando ya fue escrito y envia la información de los argumentos a text_handle
	Devuelve 0 si no ocurrio ningun error, (-1) en el caso que sí
*/
int bin_consume(char** buf, int fd, int* blen) {
	if(*buf==NULL) *buf = safe_malloc(1);
	char *args[3]= {NULL};
	int lens[3] = {0};

	// Leemos el primer byte, la instrucción, si es todavia no leyo nada
	if ((*blen) == 0) (*blen) += READ(fd, *buf, *blen, 1);

	char cmd = buf[0][0];
	switch (cmd) {
		case PUT: {
			// Leemos los dos argumentos
			READ_BINARG(fd, *buf, *blen, 1, lens[1]);
			READ_BINARG(fd, *buf, *blen, 1 + 4 + lens[1], lens[2]);
			// Parseamos los argumentos
			args[0]=*buf;
			args[1]=*buf+1+4;
			args[2]=*buf+1+4+lens[1]+4;
			// Manejamos la instrucción
			if (bin_handle(fd,args,lens) == -1) return -1;
			break;
		}
		case GET: {
			// Leemos el argumento
			READ_BINARG(fd, *buf, *blen, 1, lens[1]);
			// Parseamos los argumentos
			args[0] = *buf;
			args[1] = *buf+1+4;
			// Manejamos la instrucción
			if (bin_handle(fd,args,lens) == -1) return -1;
			break;
		}
		case DEL: {
			// Leemos el argumento
			READ_BINARG(fd, *buf, *blen, 1, lens[1]);
			// Parseamos los argumentos
			args[0] = *buf;
			args[1] = *buf+1+4;
			// Manejamos la instrucción
			if (bin_handle(fd,args,lens) == -1) return -1;
			break;
		}
		case STATS: {
			args[0] = *buf;
			// Manejamos la instrucción
			if (bin_handle(fd,args,lens) == -1) return -1;
			break;
		}
		default: {
			// La instruccion es incorrecta
			char reply = EINVALID;
			if (writen(fd, &reply, 1) == -1) return -1;
			break;
		}
	}
	// Al lograr manejar la instruccion, liberamos la memoria del buffer
	if((*buf) != NULL) {
		free(*buf);											
		*buf = NULL;
	}
	*blen = 0;
	return 0;
}

// text_handle : int, char**, int, int -> int
/*
	Maneja las instrucciones del protocolo de texto
*/
int text_handle(int fd, char *args[3], int lens[3], int nargs){
	char *cmd = args[0];
	// Instrucción PUT
    if(strcmp(cmd,"PUT") == 0) {
		// Comprobamos si la cantidad de argumentos es valida
		if(nargs != 3) {
			if (writen(fd, "EINVALID\n", 9) == -1) return -1;
			return 0;
		}
		// Realizamos las acciones necesarias
		stats_inc(table->stats, PUT_STAT);
        hashtable_insert(args[1], lens[1], args[2], lens[2], 0);

		// Devolvemos el resultado
		if (writen(fd, "OK\n", 3) == -1) return -1;
    } 
	// Instrucción GET
	else if(strcmp(cmd,"GET") == 0) {
		// Comprobamos si la cantidad de argumentos es valida
		if(nargs != 2) {
			if (writen(fd, "EINVALID\n", 9) == -1) return -1;
			return 0;
		}
		// Realizamos las acciones necesarias
		stats_inc(table->stats, GET_STAT);
		int bin=0;
        String val = hashtable_search(args[1], lens[1], &bin);
		// Devolvemos el resultado
		if(val != NULL){
			if(bin == 1) {
				if (writen(fd, "EBINARY\n", 8) == -1) return -1;
			} else{
				if (writen(fd, "OK ", 3) == -1) return -1;
				if (writen(fd, val->data, val->len) == -1) return -1;
				if (writen(fd,"\n",1) == -1) return -1;
			}
            string_destroy(val);
		}
		else if (writen(fd, "ENOTFOUND\n", 10) == -1) return -1;
	} 
	// Instrucción DEL
	else if(strcmp(cmd,"DEL") == 0) {
		// Comprobamos si la cantidad de argumentos es valida
		if(nargs != 2) {
			if (writen(fd, "EINVALID\n", 9) == -1) return -1;
			return 0;
		}
		// Realizamos las acciones necesarias
		stats_inc(table->stats, DEL_STAT);
        int res = hashtable_delete(args[1], lens[1]);
		// Devolvemos el resultado
		char reply[2024];
        if(res != -1) {
			sprintf(reply, "OK\n");
		} else
			sprintf(reply, "ENOTFOUND\n");
		if (writen(fd, reply, strlen(reply)) == -1) return -1;
    } 
	// Instrucción STATS
	else if(strcmp(cmd,"STATS") == 0) {
		// Comprobamos si la cantidad de argumentos es valida
		if(nargs != 1) {
			char reply[10];
        	sprintf(reply, "EINVALID\n");
			if (writen(fd, reply, 10) == -1) return -1;
			return 0;
		}
		// Devolvemos el resultado
		char reply[2024];
		int len = sprintf(reply, "OK PUTS=%lu GETS=%lu DELS=%lu KEYS=%lu\n", table->stats->amounts[0], table->stats->amounts[1], table->stats->amounts[2], table->stats->amounts[3]);
		if (writen(fd, reply, len) == -1) return -1;
    } 
	// La instrucción es incorrecta
	else {
		char reply[10];
		sprintf(reply, "EINVALID\n");
		if (writen(fd, reply, 10) == -1) return -1;
	}
	return 0;
}

// text_consume : char**, int, int* -> int
/*  
	Va acumulando en un buffer hasta donde puede leer de un socket
	Al leer un "\n", entiende que el comando ya fue escrito y envia
	la información de los argumentos a text_handle
	Devuelve 0 si no ocurrio ningun error, (-1) en el caso que sí
*/
int text_consume(char** buf, int fd, int* blen) {
	if((*buf)==NULL) (*buf) = safe_malloc(2048);
	while (1) {
		// int rem = sizeof *buf - blen;
		int rem = 2048 - (*blen);
		assert (rem >= 0);

		// Al superar el limite del buffer, devolvemos EINVALID y
		// consumimos la información restante hasta terminar el comando
		if (rem == 0) {
			if (writen(fd, "EINVALID\n", 9) == -1) return -1;
			if((*buf) != NULL) {
				free(*buf);											
				*buf = NULL;
			}
			*blen = 0;
			char c = 'c';
			while(c != '\n' && c != EOF) read(fd, &c, 1);
			return 0;
		}
		// Movemos los caracteres leidos del socket al buffer
		(*blen) += READ(fd, *buf, *blen, rem);	
		char *p, *p0 = (*buf);
		int nlen = (*blen);

		// Para cada \n, procesar, y avanzar punteros
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Obtuvismos el mensaje completo */
			int len = p - p0;
			*p++ = 0;
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			// Parseamos el mensaje en los argumentos
			ntok = text_parser(p0, toks, lens);

			// Manejamos la instrucción
			if (text_handle(fd, toks, lens, ntok) == -1) return -1;

			nlen -= len + 1;
			p0 = p;
		}

		/* Al haber consumido, movemos el buffer */
		if (p0 != (*buf)) {
			memmove((*buf), p0, nlen);
			(*blen) = nlen;
		}
	}
	return 0;
}

// thread : void* -> NULL
/*
	Cuerpo principal de cada thread
	Ira manejando las solicitudes de las distintas conexiones de los clientes
*/
void *thread(void *args) {
	
	int nfds, csock;
	struct epoll_event events[MAX_EVENTS];

	// Obtiene los argumentos pasados al thread
	struct ThreadArgs* thread_args = (struct ThreadArgs*)args;
    int mock_event = thread_args->mock_event;
    int text_sock = thread_args->text_sock;
	int bin_sock = thread_args->bin_sock;
    int efd = thread_args->efd;

	// Hara un loop infinito hasta que el servidor se corte
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
				if (writen(mock_event, &dummy, sizeof(uint64_t)) == -1) {
					perror("Error writing in mock");
					exit(EXIT_FAILURE);
				};
			} else if(data->fd == text_sock || data->fd == bin_sock) {
				// Si la conexión es nueva, la agrega a la lista de interes del servidor
				csock = new_client(data->fd);
				log(1, "Nuevo Cliente id:%d\n", csock);
				epoll_add(efd, csock, data->mode, EPOLLIN | EPOLLET | EPOLLONESHOT);
				epoll_mod(efd, data->fd, data->mode, data, EPOLLIN | EPOLLET | EPOLLONESHOT);
			} else {
				// Si la conexión no es nueva, maneja la instrucción solicitada por el cliente
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
	// Inicializamos el evento epoll
	int efd = epoll_init();

	// Agregamos los socket iniciales
	epoll_add(efd, mock_event, TEXT, EPOLLIN | EPOLLET);
	epoll_add(efd, text_sock, TEXT, EPOLLIN | EPOLLET | EPOLLONESHOT);
	epoll_add(efd, bin_sock, BIN, EPOLLIN | EPOLLET | EPOLLONESHOT);

	// Establecemos los argumentos de los threads
	struct ThreadArgs *args = (struct ThreadArgs*)safe_malloc(sizeof(struct ThreadArgs));
    args->mock_event = mock_event;
	args->text_sock = text_sock;
	args->bin_sock = bin_sock;
	args->efd = efd;

	// Creamos los distintos threads
	long num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    log(1,"Server running with %d threads.",num_cores);
	pthread_t threads[num_cores];
    for (int i = 0; i < num_cores; i++) {
        if (pthread_create(&threads[i], NULL, thread, args) != 0) {
            fprintf(stderr, "Error creando el hilo %d\n", i);
            exit(EXIT_FAILURE);
        }
    }

	// Esperamos a que los threads terminen, si es que ocurre algún problema
	for (int i = 0; i < num_cores; i++) {
        if (pthread_join(threads[i], NULL) != 0) {
            fprintf(stderr, "Error al esperar al hilo %d\n", i);
            exit(EXIT_FAILURE);
        }
    }

	// Liberamos la memoria creada y cerramos el epoll
	free(args);
	close(efd);
}
