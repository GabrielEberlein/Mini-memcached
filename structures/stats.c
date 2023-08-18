#include "stats.h"

#include <stdlib.h>

Stats stats_init() {
    Stats stats = malloc(sizeof(struct _Stats));
    for(int i = 0; i < NUM_STATS; i++) {
        stats->amounts[i] = 0;
        pthread_mutex_init(stats->locks+i, NULL);
    }
    return stats;
}

void stats_inc(Stats stats, int stat) {
    pthread_mutex_lock(&stats->locks[stat]);
    stats->amounts[stat]++;
    pthread_mutex_unlock(&stats->locks[stat]);
}

void stats_dec(Stats stats, int stat) {
    pthread_mutex_lock(&stats->locks[stat]);
    stats->amounts[stat]--;
    pthread_mutex_unlock(&stats->locks[stat]);
}

void stats_ret(Stats stats, int *rets) {
    for(int i = 0; i < NUM_STATS; i++) {
        pthread_mutex_lock(&stats->locks[i]);
        rets[i] = stats->amounts[i];
        pthread_mutex_unlock(&stats->locks[i]);
    }
}

void stats_destroy(Stats stats) {
    for (int i = 0; i < NUM_STATS; i++) {
        pthread_mutex_destroy(&stats->locks[i]);
    }
}
