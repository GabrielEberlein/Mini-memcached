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
#include "commons/log.h"
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

// Macro para leer del socket
/*
	Lee todo lo que puede del socket
	En el caso de completarse la lectura, devuelve > 0
	En el caso de ser interrumpido retorna 0
	En el caso de ocurrir un error de conexión o si que el cliente terminó la conexión, retorna -1
*/
#define READ(fd, buf, blen, n) ({							\
	int rc = read(fd, buf + blen, n);						\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))\
		return 0;											\
	if (rc <= 0)											\
		return -1;											\
	rc; }) 													\

// Macro para leer del socket, se va fijando que realizar a partir de cuanto se leyo del buffer
/*
	1. Se fija si no se leyo nada del argumento actual, en caso que si,
	se le aloja suficiente memoria al buffer para guardar los bytes del largo
	2. Se fija si se leyeron los bytes del largo, en el caso que no,
	lee todo lo que puede del socket
	3. Se fija si ya se leyeron los bytes del largo, en caso que si,
	se le aloja suficiente memoria al buffer para guardar los bytes de la cadena
	4. Se fija si ya se leyeron los bytes de la cadena, en caso que no,
	lee todo lo que puede del socket

	En el caso de completarse la lectura, devuelve > 0
	En el caso de ser interrumpida la lectura, se retorna 0
	En el caso de ocurrir un error de conexión o si que el cliente terminó la conexión, se retorna -1
*/
#define READ_BINARG(fd, buf, blen, off, len) ({ 											\
	if(blen==off) buf = safe_realloc(buf, off + 4); 										\
	if(buf == NULL) { 																		\
		blen=0;																				\
		char reply = ENOMEMORY;																\
		WRITEN(fd, &reply, 1);																\
		return 0;}																			\
	if (blen < off + 4){																	\
		int rc = READ(fd, buf, blen, off + 4 - blen);										\
		if(rc < off + 4 - blen)	{\
			len+=rc;				\
			return 0;}				\
		blen+=rc;}																			\
	unsigned int len_net;																	\
	memcpy(&len_net, buf + off, 4);															\
	len = ntohl(len_net);																	\
	if(blen==off + 4) buf = safe_realloc(buf, off + 4 + len);								\
	if(buf == NULL) { 																		\
		blen=0;																				\
		char reply = ENOMEMORY;																\
		WRITEN(fd, &reply, 1);																\
		return 0;}																			\
	if (blen < off + 4 + len) {																\
		int rc = READ(fd, buf, blen, off + 4 + len - blen);									\
		if(rc < off + 4 + len - blen) {														\
			blen+=rc;																		\
			return 0;}																		\
		blen+=rc;}																			\
})																							\

// Macro para comprobar si no ocurrieron errores al escribir en el socket
#define WRITEN(fd, buf, len) if (writen(fd, buf, len) == -1) return -1;

// server : int, int, int -> NULL
/*
	Inicializa el servidor, creando los distintos threads que manejaran las conexiones
*/
void server(int text_sock, int bin_sock, int mock_event);