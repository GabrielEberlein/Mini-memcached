#ifndef __HASH_H_
#define __HASH_H_

#define NUM_STATS 4

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

struct _Stats{
    int amounts[NUM_STATS];
    pthread_mutex_t locks[NUM_STATS];
};

typedef struct _Stats* Stats;

Stats stats_init();

void stats_inc(Stats stats, int stat);

void stats_ret(Stats stats, int *rets);

void stats_destroy(Stats stats);

#endif