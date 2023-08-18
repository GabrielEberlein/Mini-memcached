#ifndef __QUEUE__
#define __QUEUE__

struct _String{
  char *data;
  int len;
};

typedef struct _String* String;

struct _Node{
  String key;
  String val;
  struct _Node *left; //relaciones en el arbol
  struct _Node *right;
  struct _Node* prev; //relaciones en la cola
  struct _Node* next;
};

typedef struct _Node* Node;

typedef struct Queue {
    Node first;
    Node last;
    pthread_mutex_t lock;
} Queue;

Queue* create_queue();

void push_queue(Queue* queue, Node node);

int pop_queue(Queue* queue);

void relocate_queue(Queue* queue, Node node);

void remove_queue(Queue* queue, Node node);

#endif