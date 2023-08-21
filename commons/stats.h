#ifndef __STATS__
#define __STATS__

enum counter {
    PUTS, GETS, DELS, KEYS
};

struct _Stats {
    int counters[4];
    pthread_mutex_t locks[4];
};

typedef struct _Stats *Stats;

Stats stats_create();

void stats_destroy();

void increase(Stats stats, enum counter counter);

void decrease(Stats stats, enum counter counter);

#endif