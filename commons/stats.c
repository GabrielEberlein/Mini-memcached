#include <pthread.h>
#include <stdlib.h>
#include "stats.h"

Stats stats_create() {
    Stats stats = malloc(sizeof(struct _Stats));
    for(int i = 0; i < 4; i++) {
        stats->counters[i] = 0;
        pthread_mutex_init(&stats->locks[i], NULL);
    }
    return stats;
}

void stats_destroy(Stats stats) {
    for(int i = 0; i < 4; i++) pthread_mutex_destroy(&stats->locks[i]);
    free(stats);
}

void increase(Stats stats, enum counter counter) {
    pthread_mutex_lock(&stats->locks[counter]);
    stats->counters[counter]++;
    pthread_mutex_unlock(&stats->locks[counter]);
}

void decrease(Stats stats, enum counter counter) {
    pthread_mutex_lock(&stats->locks[counter]);
    stats->counters[counter]--;
    pthread_mutex_unlock(&stats->locks[counter]);
}