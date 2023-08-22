#ifndef __HASH_H_
#define __HASH_H_

#include "bst.h"
#include "stats.h"

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

// Estructura Tabla Hash
/*
  
*/
struct _HashTable {
  Node *elems;
  Stats stats;
  pthread_mutex_t locks[NUM_REGIONS];
  unsigned capacity;
  unsigned range;
  CompareFunction comp;
  DestructorFunction destr;
  HashFunction hash;
};
typedef struct _HashTable *HashTable;

unsigned hash_word(String word);

// hashtable_create : unsigned -> HashTable
/*
  Crea e inicializa una Tabla Hash
*/
HashTable hashtable_create(unsigned capacity);

// insert_hashtable : Queue*, HashTable, String, String -> NULL
/*
  Agrega el par (Key, Value) a la Tabla Hash
*/
void hashtable_insert(Queue queue, HashTable table, String key, String val, int bin);

// hashtable_search : Queue*, HashTable, String -> String
/*
  Busca un valor en la Tabla Hash por su Key, retorna el mismo
*/
String hashtable_search(Queue queue, HashTable table, String key, int* bin);

// hashtable_delete : Queue*, HashTable, String -> int
/*
  Borra un valor de la Tabla Hash a partir de su Key
  Si el mismo no se encuentra, se devuelve (-1), en caso contrario, 0
*/
int hashtable_delete(Queue queue, HashTable table, String key);

// hashtable_destroy : HashTable -> NULL
/*
  Destruye y borra de la memoria la Tabla Hash
*/
void hashtable_destroy(HashTable tabla);

#endif