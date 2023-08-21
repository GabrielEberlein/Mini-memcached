#ifndef __STATS_H_
#define __STATS_H_

#define NUM_STATS 4

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

// Estructura Stats
/*
    Guarda el estado del servidor
    - amounts : int[] / Lista de contadores
        [0] = Cantidad de las operaciones PUT 
        [0] = Cantidad de las operaciones GET 
        [0] = Cantidad de las operaciones DEL  
        [0] = Cantidad de los pares (Key, Value)
    - locks : pthread_mutex_t / Locks de los contadores
        [0] = Lock de las operaciones PUT 
        [0] = Lock de las operaciones GET 
        [0] = Lock de las operaciones DEL   
        [0] = Lock de los pares (Key, Value)
*/ 
struct _Stats{
    int amounts[NUM_STATS];
    pthread_mutex_t locks[NUM_STATS];
};
typedef struct _Stats *Stats;

// stats_init : NULL -> Stats
/*
    Crea e inicializa la estrcutura Stats
*/
Stats stats_init();

// stats_inc : Stats, int -> NULL
/*
    Incrementa el contador especificado
*/
void stats_inc(Stats stats, int stat);

// stats_dec : Stats, int -> NULL
/*
    Decrementa el contador especificado
*/
void stats_dec(Stats stats, int stat);

// stats_ret : Stats, int* -> NULL
/*
    Guarda en un array de enteros todos los contadores
*/
void stats_ret(Stats stats, int *rets);

// stats_destroy : Stats, int -> NULL
/*
    Destruye y borra de la memoria los Stats
*/
void stats_destroy(Stats stats);

#endif