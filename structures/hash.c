#include <assert.h>
#include <stdlib.h>
#include <pthread.h>
#include "hash.h"
#include "../commons/log.h"

static inline uint32_t murmur_32_scramble(uint32_t k) {
    k *= 0xcc9e2d51;
    k = (k << 15) | (k >> 17);
    k *= 0x1b873593;
    return k;
}

uint32_t murmur3_32(const uint8_t* key, size_t len, uint32_t seed)
{
	uint32_t h = seed;
    uint32_t k;
    /* Read in groups of 4. */
    for (size_t i = len >> 2; i; i--) {
        // Here is a source of differing results across endiannesses.
        // A swap here has no effects on hash properties though.
        memcpy(&k, key, sizeof(uint32_t));
        key += sizeof(uint32_t);
        h ^= murmur_32_scramble(k);
        h = (h << 13) | (h >> 19);
        h = h * 5 + 0xe6546b64;
    }
    /* Read the rest. */
    k = 0;
    for (size_t i = len & 3; i; i--) {
        k <<= 8;
        k |= key[i - 1];
    }
    // A swap is *not* necessary here because the preceding loop already
    // places the low bytes in the low places according to whatever endianness
    // we use. Swaps only apply when the memory is copied in a chunk.
    h ^= murmur_32_scramble(k);
    /* Finalize. */
	h ^= len;
	h ^= h >> 16;
	h *= 0x85ebca6b;
	h ^= h >> 13;
	h *= 0xc2b2ae35;
	h ^= h >> 16;
	return h;
}

unsigned hash_word(String word) {
  return murmur3_32((uint8_t*) word->data, word->len, SEED);
}

HashTable hashtable_create(unsigned capacity) {
  HashTable table = malloc(sizeof(struct _HashTable));
  assert(table != NULL);
  table->elems = calloc(capacity, sizeof(struct _Node));
  assert(table->elems != NULL);
  table->capacity = capacity;
  table->stats = stats_init();
  table->range = capacity / NUM_REGIONS;
  table->comp = (CompareFunction)string_compare;
  table->destr = (DestructorFunction)bst_destroy;
  table->hash = (HashFunction)hash_word;

  for (unsigned idx = 0; idx < NUM_REGIONS; ++idx) {
    pthread_mutex_init(table->locks+idx, NULL);
  }

  return table;
}

void hashtable_insert(Queue queue, HashTable table, String key, String val) {
  unsigned idx = table->hash(key) % table->capacity;
  log(1,"Hash: %d",idx);
  int region = idx / table->range;
  pthread_mutex_lock(table->locks+region);
  table->elems[idx] = bst_insert(queue, table->elems[idx], table->stats, key, val);
  pthread_mutex_unlock(table->locks+region);
}

String hashtable_search(Queue queue, HashTable table, String key) {
  unsigned idx = table->hash(key) % table->capacity;
  log(1,"Hash: %d",idx);
  int region = idx / table->range;
  pthread_mutex_lock(table->locks+region);
  String value = bst_search(queue, table->elems[idx], key);
  pthread_mutex_unlock(table->locks+region);
  return value;
}

int hashtable_delete(Queue queue, HashTable table, String key) {
  unsigned idx = table->hash(key) % table->capacity;
  int region = idx / table->range;
  pthread_mutex_lock(table->locks+region);
  int res = bst_delete(queue, &(table->elems[idx]), table->stats, key);
  pthread_mutex_unlock(table->locks+region);
  return res;
}

void hashtable_destroy(HashTable table) {
  // Destruir cada uno de los datos.
  for (unsigned idx = 0; idx < table->capacity; ++idx)
      table->destr(table->elems[idx]);

  // Destruir cada uno de los locks.
  for (unsigned idx = 0; idx < NUM_REGIONS; ++idx)
      pthread_mutex_destroy(table->locks+idx);

  stats_destroy(table->stats);
  // Liberar el arreglo de casillas y la Tabla.
  free(table->elems);
  free(table);
}
