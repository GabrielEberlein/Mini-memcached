#ifndef __BST_H_
#define __BST_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct _BST{
  char *key;
  char *value;
  int size;
  struct _BST *left;
  struct _BST *right;
};

typedef struct _BST* BST;

BST new_pair(char* key, char* value);

/**
 * Retorna 0 si los contactos tienen el mismo nombre.
 */
int compare_keys(char *w1, char *w2);

/**
 * Función destructora de un contacto.
 */

int size(BST node);

BST free_bst(BST node);

BST delete_node(BST node);

BST insert_bst(BST node, char* key, char* value);

int delete_bst(BST* node, char *key);

char* search_bst(BST node, char* key);

#endif /* __CONTACTO_H__ */