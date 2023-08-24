#ifndef __SAFEALLOC_H
#define __SAFEALLOC_H 1

#include "common.h"
#include "../structures/hash.h"

void* safe_malloc(size_t size, Queue queue, HashTable table);


#endif
