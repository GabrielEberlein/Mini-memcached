#ifndef __STATS_H_
#define __STATS_H_

#define NUM_STATS 4

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/types.h>

enum stat_enum {PUT_STAT, GET_STAT, DEL_STAT, KEYS_STAT};

// Estructura Stats
/*
    Guarda el estado del servidor
    - amounts : u_int64_t[] / Lista de contadores
        [0] = Cantidad de las operaciones PUT 
        [1] = Cantidad de las operaciones GET 
        [2] = Cantidad de las operaciones DEL  
        [3] = Cantidad de los pares (Key, Value)
    - locks : Lock de los contadores
*/ 
struct _Stats{
    u_int64_t amounts[NUM_STATS];
    pthread_mutex_t lock;
};
typedef struct _Stats *Stats;

// stats_init : NULL -> Stats
/*
    Crea e inicializa la estrcutura Stats
*/
Stats stats_init();

// stats_inc : Stats, enum stat_enum -> NULL
/*
    Incrementa el contador especificado
*/
void stats_inc(Stats stats, enum stat_enum stat);

// stats_dec : Stats, enum stat_enum -> NULL
/*
    Decrementa el contador especificado
*/
void stats_dec(Stats stats, enum stat_enum stat);

// stats_destroy : Stats -> NULL
/*
    Destruye y borra de la memoria los Stats
*/
void stats_destroy(Stats stats);

#endif