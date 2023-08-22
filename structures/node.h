#ifndef __NODE_H_
#define __NODE_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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
int string_compare(String k1, String k2);

// free_string : String -> NULL
/*
    Destruye y borra de la memoria el String
*/
void string_destroy(String string);

// new_pair : String, Val -> Node
/*
    Crea e inicializa un nuevo Nodo con los valores dados
*/
Node node_create(String key, String val, int bin);

// delete_node -> Queue*, Node -> Node
/*
    Destruye y borra de la memoria un Nodo, tanto del BST como de la Cola
    Retorna el arbol sin el mismo
    Si tenia dos hijos, lo reemplaza por el mayor Nodo del subarbol izquierdo
    Si tenia un hijo, lo reemplaza por el subarbol derecho
*/
void node_destroy(Node node);

#endif