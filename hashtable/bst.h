#ifndef __BST_H_
#define __BST_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct _Node{
  char *key;
  int value;
  int size;
  struct _Node *left;
  struct _Node *right;
};

typedef struct _Node* Node;

Node new_pair(char *word, int value);

/**
 * Retorna 0 si los contactos tienen el mismo nombre.
 */
int compare_keys(char *w1, char *w2);

/**
 * Función destructora de un contacto.
 */

int size(Node node);

Node free_bst(Node node);

Node delete_node(Node node);

Node insert_bst(Node node, char* key, int value);

Node delete_bst(Node node, char *key);

int search_bst(Node node, char* key);

#endif /* __CONTACTO_H__ */