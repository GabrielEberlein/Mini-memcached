#include "stats.h"
#include <stdlib.h>

Stats stats_init() {
    Stats stats = malloc(sizeof(struct _Stats));
    for(int i = 0; i < NUM_STATS; i++) {
        stats->amounts[i] = 0;
    }
    pthread_mutex_init(&stats->lock, NULL);
    return stats;
}

void stats_inc(Stats stats, enum stat_enum stat) {
    pthread_mutex_lock(&stats->lock);
    stats->amounts[stat]++;
    pthread_mutex_unlock(&stats->lock);
}

void stats_dec(Stats stats, enum stat_enum stat) {
    pthread_mutex_lock(&stats->lock);
    stats->amounts[stat]--;
    pthread_mutex_unlock(&stats->lock);
}

void stats_destroy(Stats stats) {
    pthread_mutex_destroy(&stats->lock);
    free(stats);
}
