#ifndef __HASH_H_
#define __HASH_H_

#include "bst.h"

/** Retorna una copia fisica del dato */
typedef int (*CompareFunction)(void *data1, void *data2);
/** Retorna un entero negativo si dato1 < dato2, 0 si son iguales y un entero
 * positivo si dato1 > dato2  */
typedef void *(*DestructorFunction)(void *data);
/** Libera la memoria alocada para el dato */
typedef unsigned (*HashFunction)(void *data);
/** Retorna un entero sin signo para el dato */

struct _HashTable {
  Node *elems;
  unsigned numElems;
  unsigned collisions;
  unsigned capacity;
  CompareFunction comp;
  DestructorFunction destr;
  HashFunction hash;
};
typedef struct _HashTable *HashTable;

void insert_hashtable(HashTable table, char *key, int value);

void delete_hashtable(HashTable table, char *key);

int search_hashtable(HashTable table, char *key);

HashTable hashtable_create(unsigned capacity);

HashTable rehash_table(HashTable table);

HashTable hashtable_destroy(HashTable tabla);

HashTable hash_dictionary(char **dictionary, unsigned wordAmount, unsigned scalar);

unsigned hash_word(char *word);
#endif