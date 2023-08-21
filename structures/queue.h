#ifndef __QUEUE_H_
#define __QUEUE_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"

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
Queue queue_create();

// push_queue : Queue*, Node -> NULL
/*
  Agrega un elemento al unicio de la cola
*/
void queue_push(Queue queue, Node node);

// pop_queue : Queue* -> int
/*
  Elimina el ultimo elemento de la cola
*/
void queue_pop(Queue queue);

// relocate_queue : Queue*, Node -> NULL
/*
  Recoloca el Nodo especificado en la primer posicion de la cola
*/
void queue_relocate(Queue queue, Node node);

// queue_delete : Queue*, Node -> NULL
/*
  Borra un elemento de la cola
*/
void queue_delete(Queue queue, Node node);

// destroy_list : Queue* -> int
/*
  Destruye y borra de la memoria la lista doblemente enlazada
*/
void list_destroy(Node firstNode);

// queue_destroy : Queue* -> int
/*
  Destruye y borra de la memoria la cola
*/
void queue_destroy(Queue queue);

#endif