#include <stdio.h>
#include <stdlib.h>
#include <sys/epoll.h>

enum modes {TEXT, BIN};

// Estructura Data
/*
    Guarda los datos de cada File Descriptor monitoreado por Epoll
    - fd : int / Indica el File Descriptor asociado al Socket del cliente
    - mode : enum modes / Guarda si el cliente es binario o de texto
*/
typedef struct Data {
    int fd;
    enum modes mode;
} Data;

// epoll_init : NULL -> int
/*
    Crea una nueva instancia epoll y retorna su File Descriptor
*/
int epoll_init();

// epoll_add : int, int, enum modes, int -> NULL
/*
    Agrega una nueva entrada en la lista de interes de epoll
*/
void epoll_add(int efd, int sock, enum modes mode, int events);

// epoll_mod : int, int, enum modes, Data*, int -> NULL
/*
    Modifica una entrada en la lista de interes de epoll 
*/
void epoll_mod(int efd, int sock, enum modes mode, Data* data, int events);