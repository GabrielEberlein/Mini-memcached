#ifndef __BST_H_
#define __BST_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "queue.h"
#include "node.h"
#include "stats.h"

// insert_bst : Queue, Node, String, String -> Node
/*
    Inserta un nuevo nodo en el BST o actualiza el valor si el mismo ya existe
    y reposicionandolo al frente de la cola
*/
Node bst_insert(Queue queue, Node node, Stats stats, String key, String val, int bin);

// search_bst : Queue, Node, String -> String
/*
    Busca un nodo con la misma Key
    Si lo encuentra, devuelve el valor y reposiciona el nodo al frente de la cola
    Si no lo encuentra, devuelve NULL
*/
String bst_search(Queue queue, Node node, String key, int* bin);

// delete_bst -> Queue, Node*, String -> int
/*
    Busca y borra un nodo de un BST
    Devuelve 0 si el mismo fue encontrado, (-1) en caso contrario
*/
int bst_delete(Queue queue, Node* node, Stats stats, String key);

// free_bst : Node -> NULL
/*
    Destruye y borra de la memoria un arbol BST
*/
Node bst_destroy(Node node);

#endif /* __CONTACTO_H__ */