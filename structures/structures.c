#include <assert.h>
#include <stdlib.h>
#include <pthread.h>
#include "../commons/log.h"
#include "structures.h"

HashTable table = NULL;
Queue queue = NULL;

/*--------------------------------------------------/
/             SAFE ALLOCATION FUNCTIONS             /
/--------------------------------------------------*/

void* safe_malloc(size_t size){
  void* ptr=NULL;
  while((ptr=malloc(size)) == NULL && queue->first != NULL) hashtable_delete(queue->first->key->data, queue->first->key->len);
  return ptr;
}

void* safe_realloc(void* ptr, size_t size){
  while((ptr=realloc(ptr, size)) == NULL && queue->first != NULL) hashtable_delete(queue->first->key->data, queue->first->key->len);
  return ptr;
}

/*-----------------------------------------------/
/               NODE FUNCTIONS                   /
/-----------------------------------------------*/

String string_create(char* data, int len){
  String string = malloc(sizeof(struct _String));
  if(string == NULL) return NULL;

  string->data = malloc(sizeof(char) * (len));
  if(string->data == NULL){
    free(string);
    return NULL;
  }
  memcpy(string->data, data, len);
  string->len = len;

  return string;
}

void string_destroy(String string) {
  free(string->data);
  free(string);
}

int string_compare(char* d1, int l1, char* d2, int l2) {
  if (l1 != l2) return l1 - l2;
  return memcmp(d1, d2, l1);
}

Node node_create(char* key, int keyLen, char* val, int valLen, int bin) {
  Node newNode = malloc(sizeof(struct _Node));
  if(newNode == NULL) return NULL;

  String k = string_create(key, keyLen);
  if(k == NULL){
    free(newNode); 
    return NULL;
  } 

  String v = string_create(val, valLen);
  if(v == NULL){
    free(newNode);
    string_destroy(k); 
    return NULL;
  }
  
  newNode->key = k;
  newNode->val = v;
  newNode->left = NULL;
  newNode->right = NULL;
  newNode->prev = NULL;
  newNode->next = NULL;
  newNode->binary = bin;

  return newNode;
}

void node_destroy(Node node){
  string_destroy(node->key);
  string_destroy(node->val);
  free(node);
}

/*----------------------------------------------/
/                QUEUE FUNCTIONS                /
/----------------------------------------------*/

void queue_create() {
    queue = malloc(sizeof(struct _Queue));
    queue->first = NULL;
    queue->last = NULL;
    pthread_mutex_init(&(queue->lock), NULL);
}

void queue_destroy() {
    Node firstNode = queue->first;
    Node currentNode = firstNode;
    while (currentNode != NULL) {
        Node nextNode = currentNode->next;
        free(currentNode);
        currentNode = nextNode;
    }
    pthread_mutex_destroy(&(queue->lock));
    free(queue);
}

void queue_push(Node node) {
    pthread_mutex_lock(&(queue->lock));
    if (queue->first == NULL){
        queue->first = node;
        queue->last = node;
    }else{
        queue->last->prev = node;
        node->next = queue->last;
        queue->last = node;
    }
    pthread_mutex_unlock(&(queue->lock));
}

void queue_delete(Node node){
    pthread_mutex_lock(&(queue->lock));
    if(node->prev) node->prev->next = node->next; 
    else queue->last = node->next;
    if(node->next) node->next->prev = node->prev;
    else queue->first = node->prev;
    node_destroy(node);
    pthread_mutex_unlock(&(queue->lock));
}

void queue_relocate(Node node) {
    pthread_mutex_lock(&(queue->lock));
    if (node->prev != NULL) {
        if(node->next != NULL) {
            node->next->prev = node->prev;
        }else{
            queue->first = node->prev;
        }
        node->prev->next = node->next;
        node->prev = NULL;
        node->next = queue->last;
        queue->last->prev = node;
        queue->last = node;
    }
    pthread_mutex_unlock(&(queue->lock));
}

/*--------------------------------------------/
/               BST FUNCTIONS                 /
/--------------------------------------------*/

Node bst_insert(Node root, Node newNode, Stats stats, int bin) {
  if (root == NULL){
    queue_push(newNode);
    stats_inc(stats, KEYS_STAT);
    return newNode;
  }

  int cmp = string_compare(root->key->data, root->key->len, newNode->key->data, newNode->key->len);
  if (cmp == 0) {
    newNode->left = root->left;
    newNode->right = root->right;
    queue_delete(root);
    queue_push(newNode);
    return newNode;
  }
  if (cmp > 0 ) root->left = bst_insert(root->left, newNode, stats, bin);
  if (cmp < 0 ) root->right = bst_insert(root->right, newNode, stats, bin);

  return root;
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

int bst_delete(Node* node, Stats stats, char* key, int keyLen){
  if((*node)==NULL) return -1;
  int cmp = string_compare((*node)->key->data, (*node)->key->len, key, keyLen);

  if (cmp == 0) {
    Node replacement = bst_replace((*node));
    queue_delete((*node));
    (*node) = replacement;
    stats_dec(stats, KEYS_STAT);
  }
  if (cmp > 0 ) return bst_delete(&((*node)->left), stats, key, keyLen);
  if (cmp < 0 ) return bst_delete(&((*node)->right), stats, key, keyLen);

  return 0;
}

Node bst_search(Node node, char* key, int keyLen){
  if(node == NULL) return NULL;
  int cmp = string_compare(key, keyLen, node->key->data, node->key->len);
  if (cmp == 0) {
    queue_relocate(node);
    return node;
  }
  if (cmp < 0 ) return bst_search(node->left, key, keyLen);
  if (cmp > 0 ) return bst_search(node->right, key, keyLen);
  return NULL;
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

/*---------------------------------------------------/
/               HASH TABLE FUNCTIONS                 /
/---------------------------------------------------*/

static inline uint32_t murmur_32_scramble(uint32_t k) {
    k *= 0xcc9e2d51;
    k = (k << 15) | (k >> 17);
    k *= 0x1b873593;
    return k;
}

uint32_t murmur3_32(const uint8_t* key, size_t len, uint32_t seed)
{
	uint32_t h = seed;
    uint32_t k;
    /* Read in groups of 4. */
    for (size_t i = len >> 2; i; i--) {
        // Here is a source of differing results across endiannesses.
        // A swap here has no effects on hash properties though.
        memcpy(&k, key, sizeof(uint32_t));
        key += sizeof(uint32_t);
        h ^= murmur_32_scramble(k);
        h = (h << 13) | (h >> 19);
        h = h * 5 + 0xe6546b64;
    }
    /* Read the rest. */
    k = 0;
    for (size_t i = len & 3; i; i--) {
        k <<= 8;
        k |= key[i - 1];
    }
    // A swap is *not* necessary here because the preceding loop already
    // places the low bytes in the low places according to whatever endianness
    // we use. Swaps only apply when the memory is copied in a chunk.
    h ^= murmur_32_scramble(k);
    /* Finalize. */
	h ^= len;
	h ^= h >> 16;
	h *= 0x85ebca6b;
	h ^= h >> 13;
	h *= 0xc2b2ae35;
	h ^= h >> 16;
	return h;
}

void hashtable_create(unsigned capacity) {
  table = malloc(sizeof(struct _HashTable));
  assert(table != NULL);
  table->elems = calloc(capacity, sizeof(struct _Node));
  assert(table->elems != NULL);
  table->capacity = capacity;
  table->stats = stats_init();
  table->range = capacity / NUM_REGIONS;
  table->comp = (CompareFunction)string_compare;
  table->destr = (DestructorFunction)bst_destroy;
  table->hash = (HashFunction)hash_word;

  for (unsigned idx = 0; idx < NUM_REGIONS; ++idx) {
    pthread_mutex_init(table->locks+idx, NULL);
  }
}

void hashtable_destroy() {
  // Destruir cada uno de los datos.
  for (unsigned idx = 0; idx < table->capacity; ++idx)
      table->destr(table->elems[idx]);

  // Destruir cada uno de los locks.
  for (unsigned idx = 0; idx < NUM_REGIONS; ++idx)
      pthread_mutex_destroy(table->locks+idx);

  stats_destroy(table->stats);
  // Liberar el arreglo de casillas y la Tabla.
  free(table->elems);
  free(table);
}

unsigned hash_word(char* data, int len) {
  return murmur3_32((uint8_t*) data, len, SEED);
}

void hashtable_insert(char* key, int keyLen, char* val, int valLen, int bin) {
  Node newNode = hashtable_search(key, keyLen);

  if(newNode == NULL){
    while((newNode = node_create(key, keyLen, val, valLen, bin))==NULL && queue->first != NULL){
      hashtable_delete(queue->first->key->data, queue->first->key->len);
      log(1,"LOOP");
    }
    unsigned idx = table->hash(key, keyLen) % table->capacity;
    int region = idx / table->range;
    pthread_mutex_lock(table->locks+region);
    table->elems[idx] = bst_insert(table->elems[idx], newNode, table->stats, bin);
    pthread_mutex_unlock(table->locks+region);
  }else{
    newNode->binary = bin;
    string_destroy(newNode->val);
    newNode->val = string_create(val, valLen);
  }

}

int hashtable_delete(char* key, int keyLen) {
  unsigned idx = table->hash(key, keyLen) % table->capacity;
  int region = idx / table->range;
  pthread_mutex_lock(table->locks+region);
  int res = bst_delete(&(table->elems[idx]), table->stats, key, keyLen);
  pthread_mutex_unlock(table->locks+region);
  return res;
}

Node hashtable_search(char* key, int keyLen) {
  unsigned idx = table->hash(key, keyLen) % table->capacity;
  log(1,"Hash: %d",idx);
  int region = idx / table->range;
  pthread_mutex_lock(table->locks+region);
  Node value = bst_search(table->elems[idx], key, keyLen);
  pthread_mutex_unlock(table->locks+region);
  return value;
}

