#include <assert.h>
#include <stdlib.h>
#include "hash.h"

#define SEED 0

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

unsigned hash_word(char *word) {
  return murmur3_32((uint8_t*) word, strlen(word), SEED);
}

HashTable hashtable_create(unsigned capacity) {
  // Pedimos memoria para la estructura principal y las casillas.
  HashTable table = malloc(sizeof(struct _HashTable));
  assert(table != NULL);
  table->elems = malloc(sizeof(struct _Node) * capacity);
  assert(table->elems != NULL);
  table->numElems = 0;
  table->collisions = 0;
  table->capacity = capacity;
  table->comp = (CompareFunction)compare_keys;
  table->destr = (DestructorFunction)free_bst;
  table->hash = (HashFunction)hash_word;

  // Inicializamos las casillas con datos nulos.
  for (unsigned idx = 0; idx < capacity; ++idx) {
    table->elems[idx] = NULL;
  }

  return table;
}

void insert_bst_hashtable (HashTable table, Node bst) {
  if (bst == NULL) return;

  insert_hashtable(table, bst->key, bst->value);
  insert_bst_hashtable(table, bst->left);
  insert_bst_hashtable(table, bst->right);
}

HashTable rehash_table(HashTable table){
  HashTable newTable = hashtable_create(table->capacity * 2);

  for(unsigned i=0; i < table->capacity; i++){
    insert_bst_hashtable(newTable, table->elems[i]);
  }

  hashtable_destroy(table);
  return newTable;
}

HashTable hashtable_destroy(HashTable table) {
  if(table == NULL) return NULL;
  // Destruir cada uno de los datos.
  for (unsigned idx = 0; idx < table->capacity; ++idx)
      table->destr(table->elems[idx]);

  // Liberar el arreglo de casillas y la table.
  free(table->elems);
  free(table);
  table = NULL;
  return table;
}

void insert_hashtable(HashTable table, char *key, int value){
  unsigned idx = table->hash(key) % table->capacity;

  if(table->elems[idx] != NULL){
    table->elems[idx] = insert_bst(table->elems[idx], key, value);
    table->collisions++;
  }else{
    table->elems[idx] = new_pair(key, value);
  }
  table->numElems++;
}

void delete_hashtable(HashTable table, char* key){
  unsigned idx = table->hash(key) % table->capacity;
  table->numElems -= size(table->elems[idx]);
  table->elems[idx] = delete_bst(table->elems[idx], key);
  table->numElems += size(table->elems[idx]);
}

int search_hashtable(HashTable table, char *key){
  unsigned idx = table->hash(key) % table->capacity;
  return search_bst(table->elems[idx], key);
}