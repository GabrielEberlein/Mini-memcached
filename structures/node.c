#include <assert.h>
#include "node.h"

String string_create(char* data, int len){
  String string = malloc(sizeof(struct _String));
  assert(string != NULL);

  string->data = malloc(sizeof(char) * (len));
  assert(string->data != NULL);
  memcpy(string->data, data, len);
  string->len = len;

  return string;
}

int string_compare(String k1, String k2) {
  if (k1->len != k2->len) return k1->len - k2->len;
  return memcmp(k1->data, k2->data, k1->len);
}

void string_destroy(String string) {
  free(string->data);
  free(string);
}

Node node_create(String key, String val) {
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

void node_destroy(Node node){
  string_destroy(node->key);
  string_destroy(node->val);
  free(node);
}