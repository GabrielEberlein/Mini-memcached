#ifndef __NODE_H_
#define __NODE_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "stats.h"

/*--------------------------------------------------/
/             SAFE ALLOCATION FUNCTIONS             /
/--------------------------------------------------*/

void* safe_malloc(size_t size);
void* safe_realloc(void* ptr, size_t size);

// Estructura String
/*
  Representa un String
  - data : el string en si
  - len : el largo del mismo
*/
struct _String{
  char *data;
  int len;
};
typedef struct _String* String;

// Estructura Node
/*
  Nodo que guarda los pares (key, value) del servidor
  El mismo se utiliza tanto en una lista doblemente enlazada para la cola de
  ultimo acceso como para los BST ubicados sobre cada casilla de la tabla hash
  - key : String / Key del valo
  - val : String / Valor a guardar
  - left : Node* / Raiz del subarbol izquierdo en los BST
  - right : Node* / Raiz del subarbol derecho en los BST
  - prev : Node* / Anterior nodo en la lista cola
  - next : Node* / Posterior nodo en la lista cola
*/
struct _Node{
  String key;
  String val;
  struct _Node *left; //relaciones en el arbol
  struct _Node *right;
  struct _Node* prev; //relaciones en la cola
  struct _Node* next;
  int binary;
};
typedef struct _Node* Node;

// build_string : char*, int -> String
/*
    Crea e iniciliza un nuevo String con los valores dados
*/
String string_create(char* data, int len);

// compare_keys -> String, String -> int
/*
    Compara dos String, devuelve
    - 0 si son iguales
    - 1 si el primero es mayor
    - (-1) si el segundo es mayor
*/
int string_compare(char* d1, int l1, char* d2, int l2);

// free_string : String -> NULL
/*
    Destruye y borra de la memoria el String
*/
void string_destroy(String string);

// new_pair : String, Val -> Node
/*
    Crea e inicializa un nuevo Nodo con los valores dados
*/
Node node_create(char* key, int keyLen, char* val, int valLen, int bin);

// delete_node -> Queue*, Node -> Node
/*
    Destruye y borra de la memoria un Nodo, tanto del BST como de la Cola
    Retorna el arbol sin el mismo
    Si tenia dos hijos, lo reemplaza por el mayor Nodo del subarbol izquierdo
    Si tenia un hijo, lo reemplaza por el subarbol derecho
*/
void node_destroy(Node node);

/*----------------------------------------------/
/                QUEUE FUNCTIONS                /
/----------------------------------------------*/

// Estructura Cola
/*
  Cola de elementos ordenados a partir de su ultimo acceso
  Se utiliza una lista doblemente enlazada para agrupar sus elementos
  - first : Node / Primer elemento de la cola
  - last : Node / Ultimo elemento de la cola
  - lock : pthread_mutex_t / Lock de la cola
*/
struct _Queue {
  Node first;
  Node last;
  pthread_mutex_t lock;
};
typedef struct _Queue* Queue;

// create_queue : NULL -> Queue*
/*
  Crea e inicializa una cola
*/
void queue_create();

// queue_destroy : Queue* -> int
/*
  Destruye y borra de la memoria la cola
*/
void queue_destroy();

// push_queue : Queue*, Node -> NULL
/*
  Agrega un elemento al unicio de la cola
*/
void queue_push(Node node);

// queue_delete : Queue*, Node -> NULL
/*
  Borra un elemento de la cola
*/
void queue_delete(Node node);


// relocate_queue : Queue*, Node -> NULL
/*
  Recoloca el Nodo especificado en la primer posicion de la cola
*/
void queue_relocate(Node node);

/*--------------------------------------------/
/               BST FUNCTIONS                 /
/--------------------------------------------*/

// insert_bst : Queue, Node, String, String -> Node
/*
    Inserta un nuevo nodo en el BST o actualiza el valor si el mismo ya existe
    y reposicionandolo al frente de la cola
*/
Node bst_insert(Node root, Node newNode, Stats stats);

// delete_bst -> Queue, Node*, String -> int
/*
    Busca y borra un nodo de un BST
    Devuelve 0 si el mismo fue encontrado, (-1) en caso contrario
*/
int bst_delete(Node* node, Stats stats, char* key, int keyLen);

// search_bst : Queue, Node, String -> String
/*
    Busca un nodo con la misma Key
    Si lo encuentra, devuelve el valor y reposiciona el nodo al frente de la cola
    Si no lo encuentra, devuelve NULL
*/
Node bst_search(Node node, char* key, int keyLen);

// free_bst : Node -> NULL
/*
    Destruye y borra de la memoria un arbol BST
*/
Node bst_destroy(Node node);

/*--------------------------------------------------------/
/               HASH TABLE FUNCTIONS                      /
/--------------------------------------------------------*/

#define SEED 0
#define NUM_REGIONS 1

/** Retorna una copia fisica del dato */
typedef int (*CompareFunction)(void *data1, void *data2);
/** Retorna un entero negativo si dato1 < dato2, 0 si son iguales y un entero
 * positivo si dato1 > dato2  */
typedef void *(*DestructorFunction)(void *data);
/** Libera la memoria alocada para el dato */
typedef unsigned (*HashFunction)(void *data, int len);
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

unsigned hash_word(char* data, int len);

// hashtable_create : unsigned -> HashTable
/*
  Crea e inicializa una Tabla Hash
*/
void hashtable_create(unsigned capacity);

// insert_hashtable : Queue*, HashTable, String, String -> NULL
/*
  Agrega el par (Key, Value) a la Tabla Hash
*/
void hashtable_insert(char* key, int keyLen, char* val, int valLen, int bin);

// hashtable_search : Queue*, HashTable, String -> String
/*
  Busca un valor en la Tabla Hash por su Key, retorna el mismo
*/
Node hashtable_search(char* key, int keyLen);

// hashtable_delete : Queue*, HashTable, String -> int
/*
  Borra un valor de la Tabla Hash a partir de su Key
  Si el mismo no se encuentra, se devuelve (-1), en caso contrario, 0
*/
int hashtable_delete(char* key, int keyLen);

// hashtable_destroy : HashTable -> NULL
/*
  Destruye y borra de la memoria la Tabla Hash
*/
void hashtable_destroy();

extern HashTable table;
extern Queue queue;

#endif