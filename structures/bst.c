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

Node new_pair(String key, String val) {
  Node newNode = malloc(sizeof(struct _Node));
  assert(newNode != NULL);

  newNode->key = key;
  newNode->val = val;
  newNode->left = NULL;
  newNode->right = NULL;
  newNode->prev = NULL;
  newNode->next = NULL;

  return newNode;
}

Node insert_bst(Queue* queue, Node node, String key, String val) {
  if (node==NULL){
    Node res = new_pair(key, val);
    push_queue(queue, res);
    return res;
  }

  int cmp = compare_keys(node->key, key);
  if (cmp == 0) {
    node->val->data = val->data;
    node->val->len = val->len;
    relocate_queue(queue, node);
  }
  if (cmp > 0 ) node->left  = insert_bst(queue, node->left, key, val);
  if (cmp < 0 ) node->right = insert_bst(queue, node->right, key, val);

  return node;
}

Node delete_node(Queue* queue, Node node){
  remove_queue(queue, node);

  Node replacement;
  if(node->left){
    Node prev = NULL;
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
  free_string(node->key);
  free_string(node->val);
  free(node);
  return replacement;
}

int delete_bst(Queue* queue, Node* node, String key){
  if((*node)==NULL) return -1;
  int cmp = compare_keys((*node)->key, key);

  if (cmp == 0) (*node) = delete_node(queue, (*node));
  if (cmp > 0 ) return delete_bst(queue, &((*node)->left), key);
  if (cmp < 0 ) return delete_bst(queue, &((*node)->right), key);

  return 0;
}

String search_bst(Queue* queue, Node node, String key){
  if(node == NULL) return NULL;
  int cmp = compare_keys(key, node->key);
  if (cmp == 0) {
    relocate_queue(queue, node);
    return node->val;
  }
  if (cmp < 0 ) return search_bst(queue, node->left, key);
  if (cmp > 0 ) return search_bst(queue, node->right, key);
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
Node free_bst(Node node) {
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