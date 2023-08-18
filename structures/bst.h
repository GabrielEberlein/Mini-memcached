#ifndef __BST_H_
#define __BST_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "queue.h"

String build_string(char* data, int len);

Node new_pair(String key, String val);

/*
 * Retorna 0 si los contactos tienen el mismo nombre.
 */
int compare_keys(String k1, String k2);

/*
 * Función destructora de un contacto.
 */

Node free_bst(Node node);

Node delete_node(Queue* queue, Node node);

Node insert_bst(Queue* queue, Node node, String key, String val);

int delete_bst(Queue* queue, Node* node, String key);

String search_bst(Queue* queue, Node node, String key);

#endif /* __CONTACTO_H__ */