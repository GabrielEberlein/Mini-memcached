#include "bst.h"

#define SEED 0
/**
 * Crea un nuevo contacto.
 */
BST new_pair(char *key, char *value) {
  BST newNode = malloc(sizeof(struct _BST));
  assert(newNode != NULL);

  newNode->key = malloc(sizeof(char) * (strlen(key) + 1));
  assert(newNode->key != NULL);
  strcpy(newNode->key, key);
  newNode->value = malloc(sizeof(char) * (strlen(value) + 1));
  assert(newNode->value != NULL);
  strcpy(newNode->value, value);
  newNode->left = NULL;
  newNode->right = NULL;
  newNode->size = 1;

  return newNode;
}

int size(BST node){
  if(node==NULL) return 0;
  return node->size;
}

BST insert_bst(BST node, char* key, char* value) {
  if (node==NULL) return new_pair(key, value);

  int cmp = strcmp(node->key, key);
  if (cmp == 0) node->value = value;
  if (cmp > 0 ) node->left  = insert_bst(node->left, key, value);
  if (cmp < 0 ) node->right = insert_bst(node->right, key, value);
  
  node->size = size(node->left) + size(node->right) + 1;

  return node;
}

BST delete_node(BST node){
  BST replacement;
  if(node->left){
    BST prev = NULL;
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

int delete_bst(BST* node, char* key){
  if((*node)==NULL) return -1;
  int cmp = strcmp((*node)->key, key);

  if (cmp == 0) (*node) = delete_node((*node));
  if (cmp > 0 ) return delete_bst(&((*node)->left), key);
  if (cmp < 0 ) return delete_bst(&((*node)->right), key);

  return 0;
}

char* search_bst(BST node, char *key){
  if(node == NULL) return NULL;
  int cmp = strcmp(node->key, key);
  if (cmp == 0) return node->value;
  if (cmp > 0 ) return search_bst(node->left, key);
  if (cmp < 0 ) return search_bst(node->right, key);
  return NULL;
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
BST free_bst(BST node) {
    if(node != NULL){
        free(node->key);
        free(node->value);
        free_bst(node->left);
        free_bst(node->right);
        free(node);
        node = NULL;
    }

    return node;
}