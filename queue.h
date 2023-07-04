#ifndef __QUEUE__
#define __QUEUE__

typedef struct Node {
    struct Node* next;
    struct Node* prev;
    int data;
} Node;

typedef struct Queue {
    Node* first;
    Node* last;
} Queue;

Queue* createQueue();

Node* createNode(int data);

void pushQueue(Queue* queue, int data);

int popQueue(Queue* queue);

#endif