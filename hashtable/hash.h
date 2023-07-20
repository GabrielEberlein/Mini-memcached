#ifndef __HASH_H_
#define __HASH_H_

#include "bst.h"
#define SEED 0
#define NUM_REGIONS 1000

/** Retorna una copia fisica del dato */
typedef int (*CompareFunction)(void *data1, void *data2);
/** Retorna un entero negativo si dato1 < dato2, 0 si son iguales y un entero
 * positivo si dato1 > dato2  */
typedef void *(*DestructorFunction)(void *data);
/** Libera la memoria alocada para el dato */
typedef unsigned (*HashFunction)(void *data);
/** Retorna un entero sin signo para el dato */

struct _HashTable {
  BST *elems;
  pthread_mutex_t locks[NUM_REGIONS];
  unsigned capacity;
  unsigned range;
  CompareFunction comp;
  DestructorFunction destr;
  HashFunction hash;
};
typedef struct _HashTable *HashTable;

void insert_hashtable(HashTable table, char *key, char *value);

int delete_hashtable(HashTable table, char *key);

char* search_hashtable(HashTable table, char *key);

HashTable hashtable_create(unsigned capacity);

HashTable rehash_table(HashTable table);

HashTable hashtable_destroy(HashTable tabla);

HashTable hash_dictionary(char **dictionary, unsigned wordAmount, unsigned scalar);

unsigned hash_word(char *word);
#endif