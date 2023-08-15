#include "bst.h"
#include "../commons/log.h"

#define SEED 0

String build_string(char* data, int len){
  String string = malloc(sizeof(struct _String));
  assert(string != NULL);

  string->data = malloc(sizeof(char) * (len));
  assert(string->data != NULL);
  memcpy(string->data, data, len);
  string->len = len;

  return string;
}

String free_string(String string){
  free(string->data);
  free(string);
  return NULL;
}

BST new_pair(String key, String val) {
  BST newNode = malloc(sizeof(struct _BST));
  assert(newNode != NULL);

  newNode->key = key;
  newNode->val = val;
  newNode->left = NULL;
  newNode->right = NULL;

  return newNode;
}

BST insert_bst(BST node, String key, String val) {
  if (node==NULL) return new_pair(key, val);

  int cmp = compare_keys(node->key, key);
  if (cmp == 0) node->val->data = val->data;
  if (cmp > 0 ) node->left  = insert_bst(node->left, key, val);
  if (cmp < 0 ) node->right = insert_bst(node->right, key, val);

  return node;
}

BST delete_node(BST node){
  BST replacement;
  if(node->left){
    BST prev = NULL;
    replacement = node->left;
    while(replacement->right){ 
      prev = replacement;
      replacement = replacement->right;
    }
    if(prev) prev->right = replacement->left;
    replacement->left = node->left;
    replacement->right = node->right;
  }else{
    replacement = node->right;
  }
  free(node->key);
  free(node);
  return replacement;
}

int delete_bst(BST* node, String key){
  if((*node)==NULL) return -1;
  int cmp = compare_keys((*node)->key, key);

  if (cmp == 0) (*node) = delete_node((*node));
  if (cmp > 0 ) return delete_bst(&((*node)->left), key);
  if (cmp < 0 ) return delete_bst(&((*node)->right), key);

  return 0;
}

String search_bst(BST node, String key){
  if(node == NULL) return NULL;
  int cmp = compare_keys(key, node->key);
  if (cmp == 0) return node->val;
  if (cmp < 0 ) return search_bst(node->left, key);
  if (cmp > 0 ) return search_bst(node->right, key);
  return NULL;
}

/**
 * Retorna 0 si los contactos tienen la misma palabra.
 */
int compare_keys(String k1, String k2) {
  if (k1->len != k2->len) return k1->len - k2->len;
  return memcmp(k1->data, k2->data, k1->len);
}

/**
 * Función destructora de un contacto.
 */
BST free_bst(BST node) {
    if(node != NULL){
        free_string(node->key);
        free_string(node->val);
        free_bst(node->left);
        free_bst(node->right);
        free(node);
        node = NULL;
    }

    return node;
}