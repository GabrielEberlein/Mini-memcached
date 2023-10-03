#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/eventfd.h>
#include <signal.h>
#include "commons/parser.h"
#include "commons/sock.h"
#include "server.h"

int mock_event;

// limit_mem : size_t -> NULL
/*
	Limita la memoria del servidor
*/
void limit_mem(size_t limit)
{
	struct rlimit mem_limit;

    if (getrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("getrlimit");
        exit(EXIT_FAILURE);
    }

	mem_limit.rlim_max = limit;
	mem_limit.rlim_cur = limit;
	printf("Memory limit: %zu\n", limit);

    if (setrlimit(RLIMIT_AS, &mem_limit) == -1) {
        perror("setrlimit");
        exit(EXIT_FAILURE);
    }
}

// handle_signals : int -> NULL
/*
	Maneja varias señales
*/
void handle_signals(int s) {
	switch (s)
	{
	case SIGPIPE:{
		// Ignoramos la señal para que no se termine el servidor de forma abrupta
		log(1, "SIGPIPE ERROR");
		break;
	}
	case SIGTSTP:{
		// Si se cancela el servidor por consola, se lo notificamos a los threads
		log(1, "SIGSTP");
		terminate_threads=1;
		uint64_t dummy=50;
		int n = writen(mock_event, &dummy, sizeof(uint64_t));
		assert(n==sizeof(uint64_t));
		break;
	}
	case SIGINT:{
		// Si se cancela el servidor por consola, se lo notificamos a los threads
		log(1, "SIGINT");
		terminate_threads=1;
		uint64_t dummy=50;
		int n = writen(mock_event, &dummy, sizeof(uint64_t));
		assert(n==sizeof(uint64_t));
		break;
	}
	default:
		break;
	}
}

// Función principal del servidor
int main(int argc, char **argv)
{
	int text_sock, bin_sock;

	__loglevel = 2;
	
	//Magic number: 92870000 4 threads, 1 key
	// Establece el limite de memoria
	limit_mem(536870912);

	// 
	signal(SIGPIPE, handle_signals);
	signal(SIGTSTP, handle_signals);
	signal(SIGINT, handle_signals);

	// Crea el socket del protocolo de texto
	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	// Crea el socket del protocolo binario
	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	// Nos aseguramos de remover los privilegios
	if (setuid(1000) == -1) {
		quit("setuid");
	}

	// Creo evento mock
	mock_event = eventfd(0,0);
	if (mock_event < 0)
		quit("eventfd");

	// Crea la tabla Hash
	hashtable_create(1000000);

	// Crea la Cola
	queue_create();

	// Inicializa el servidor
	server(text_sock, bin_sock, mock_event);

	// Borra de memoria las estructuras creadas
	queue_destroy();
	hashtable_destroy();
	return 0;
}