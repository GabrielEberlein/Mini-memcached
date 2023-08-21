#ifndef __SOCK_H
#define __SOCK_H 1

#include <netinet/in.h>

// mk_tcp_sock : in_port_t -> int
/*
    Crea un nuevo Socket y restorna su File Descriptor
*/
int mk_tcp_sock(in_port_t port);

// new_client : int -> int
/*
    Acepta una nueva conección al socket especificado y devuelve su File Descriptor
*/
int new_client(int sock);

#endif
