#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "safealloc.h"

void* safe_malloc(size_t size, Queue queue, HashTable table){
    void* ptr=NULL;
    while((ptr=malloc(size)) == NULL && queue->first != NULL) hashtable_delete(queue, table, queue->first->key);
    return ptr;
}

void* safe_realloc(void* ptr, size_t size, Queue queue, HashTable table){
    while((ptr=realloc(ptr, size)) == NULL && queue->first != NULL) hashtable_delete(queue, table, queue->first->key);
    return ptr;
}


