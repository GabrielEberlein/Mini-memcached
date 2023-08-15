#ifndef __BST_H_
#define __BST_H_

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct _String{
  char *data;
  int len;
};

typedef struct _String* String;

struct _BST{
  String key;
  String val;
  struct _BST *left;
  struct _BST *right;
};

typedef struct _BST* BST;

String build_string(char* data, int len);

BST new_pair(String key, String val);

/**
 * Retorna 0 si los contactos tienen el mismo nombre.
 */
int compare_keys(String k1, String k2);

/**
 * Función destructora de un contacto.
 */

BST free_bst(BST node);

BST delete_node(BST node);

BST insert_bst(BST node, String key, String val);

int delete_bst(BST* node, String key);

String search_bst(BST node, String key);

#endif /* __CONTACTO_H__ */