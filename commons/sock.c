#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <fcntl.h>
#include "common.h"

int mk_tcp_sock(in_port_t port)
{
	int s, rc;
	struct sockaddr_in sa;
	int yes = 1;

	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		quit("socket");

	rc = setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);
	if (rc != 0)
		quit("setsockopt");

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	rc = bind(s, (struct sockaddr *)&sa, sizeof sa);
	if (rc < 0)
		quit("bind");

	rc = listen(s, 10);
	if (rc < 0)
		quit("listen");

	return s;
}

// isnonblocking : int -> sfd
/*
    Configura a un Socket como no bloqueante
*/
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

int new_client(int sock){
	int csock = accept(sock, NULL, NULL);
	if(csock == -1) {
        perror("accept");
        exit(EXIT_FAILURE);
    }
	isnonblocking(csock);
	return csock;
}

