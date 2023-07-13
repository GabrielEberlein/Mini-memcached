#ifndef __QUEUE__
#define __QUEUE__

typedef struct _Node {
    struct _Node* next;
    struct _Node* prev;
    int data;
} _Node;

typedef struct _Node* Node;

typedef struct Queue {
    Node first;
    Node last;
} Queue;

Queue* create_queue();

Node create_node(int data);

void push_queue(Queue* queue, int data);

int pop_queue(Queue* queue);

#endif