#include "bst.h"

#define SEED 0
/**
 * Crea un nuevo contacto.
 */
Node new_pair(char *key, int value) {
  Node newNode = malloc(sizeof(struct _Node));
  assert(newNode != NULL);

  newNode->key = malloc(sizeof(char) * (strlen(key) + 1));
  newNode->value = value;
  strcpy(newNode->key, key);
  newNode->left = NULL;
  newNode->right = NULL;
  newNode->size = 1;

  return newNode;
}

int size(Node node){
  if(node==NULL) return 0;
  return node->size;
}

Node insert_bst(Node node, char* key, int value) {
  if (node==NULL) return new_pair(key, value);

  int cmp = strcmp(node->key, key);
  if (cmp == 0) node->value = value;
  if (cmp > 0 ) node->left  = insert_bst(node->left, key, value);
  if (cmp < 0 ) node->right = insert_bst(node->right, key, value);
  
  node->size = size(node->left) + size(node->right) + 1;

  return node;
}

Node delete_node(Node node){
  Node replacement;
  if(node->left){
    Node prev = NULL;
    replacement = node->left;
    while(replacement->right){ 
      prev = replacement;
      prev->size--;
      replacement = replacement->right;
    }
    if(prev) prev->right = replacement->left;
    replacement->left = node->left;
    replacement->right = node->right;
    replacement->size = replacement->left->size + replacement->right->size + 1;
  }else{
    replacement = node->right;
  }
  free(node->key);
  free(node);
  return replacement;
}

Node delete_bst(Node node, char* key){
  if(node==NULL) return NULL;
  int cmp = strcmp(node->key, key);

  if (cmp == 0) node = delete_node(node);
  if (cmp > 0 ) node->left  = delete_bst(node->left, key);
  if (cmp < 0 ) node->right = delete_bst(node->right, key);

  return node;
}

int search_bst(Node node, char *key){
  if(node == NULL) return -1;
  int cmp = strcmp(node->key, key);
  if (cmp == 0) return node->value;
  if (cmp > 0 ) return search_bst(node->left, key);
  if (cmp < 0 ) return search_bst(node->right, key);
}

/**
 * Retorna 0 si los contactos tienen la misma palabra.
 */
int compare_keys(char *k1, char *k2) {
  return strcmp(k1, k2);
}

/**
 * Función destructora de un contacto.
 */
Node free_bst(Node node) {
    if(node != NULL){
        free(node->key);
        free_bst(node->left);
        free_bst(node->right);
        free(node);
        node = NULL;
    }

    return node;
}