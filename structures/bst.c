#include "bst.h"
#include "../commons/log.h"

#define SEED 0

Node bst_insert(Queue queue, Node node, Stats stats, String key, String val, int bin) {
  if (node == NULL){
    Node res = node_create(key, val, bin);
    queue_push(queue, res);
    stats_inc(stats, KEYS_STAT);
    return res;
  }

  int cmp = string_compare(node->key, key);
  if (cmp == 0) {
    node->val->data = val->data;
    node->val->len = val->len;
    queue_relocate(queue, node);
  }
  if (cmp > 0 ) node->left  = bst_insert(queue, node->left, stats, key, val, bin);
  if (cmp < 0 ) node->right = bst_insert(queue, node->right, stats, key, val, bin);

  return node;
}

String bst_search(Queue queue, Node node, String key, int* bin){
  if(node == NULL) return NULL;
  int cmp = string_compare(key, node->key);
  if (cmp == 0) {
    queue_relocate(queue, node);
    (*bin) = node->binary;
    return node->val;
  }
  if (cmp < 0 ) return bst_search(queue, node->left, key, bin);
  if (cmp > 0 ) return bst_search(queue, node->right, key, bin);
  return NULL;
}

Node bst_replace(Node node) {
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
    } else {
      replacement = node->right;
    }
  return replacement;
}

int bst_delete(Queue queue, Node* node, Stats stats, String key){
  if((*node)==NULL) return -1;
  int cmp = string_compare((*node)->key, key);

  if (cmp == 0) {
    Node replacement = bst_replace((*node));
    queue_delete(queue, (*node));
    (*node) = replacement;
    stats_dec(stats, KEYS_STAT);
  }
  if (cmp > 0 ) return bst_delete(queue, &((*node)->left), stats, key);
  if (cmp < 0 ) return bst_delete(queue, &((*node)->right), stats, key);

  return 0;
}

Node bst_destroy(Node node) {
  if(node != NULL){
      string_destroy(node->key);
      string_destroy(node->val);
      bst_destroy(node->left);
      bst_destroy(node->right);
      free(node);
      node = NULL;
  }
  return node;
}