#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

Queue* createQueue() {
    Queue* queue = (Queue*)malloc(sizeof(Queue));
    queue->first = NULL;
    queue->last = NULL;
    return queue;
}

Node* createNode(int data) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->next = NULL;
    newNode->prev = NULL;
    newNode->data = data;
    return newNode;
}

void pushQueue(Queue* queue, int data) {
    Node* newNode = createNode(data);
    if (queue->first == NULL) {
        queue->first = newNode;
        queue->last = newNode;
    } else {
        newNode->prev = queue->last;
        queue->last->next = newNode;
        queue->last = newNode;
    }
}

int popQueue(Queue* queue) {
    if (queue->first == NULL) return -1;
    Node* lastNode = queue->last;
    int lastData = lastNode->data;
    if(lastNode->prev == NULL) {
        queue->first = NULL;
        queue->last = NULL;
    } else {
        queue->last = lastNode->prev;
        queue->last->next = NULL;
    }
    free(lastNode);
    return lastData;
}

int relocateQueue(Queue* queue, Node* node) {
    if (node->prev != NULL) {
        if(node->next != NULL) {
            node->next->prev = node->prev;
        }
        node->prev->next = node->next;
        queue->last = node->prev;
        node->prev = NULL;
        node->next = queue->first;
        queue->first = node;
    } 
    return node->data;
}

void destroyList(Node *firstNode) {
    Node* currentNode = firstNode;
    while (currentNode != NULL) {
        Node* nextNode = currentNode->next;
        free(currentNode);
        currentNode = nextNode;
    }
}

void destroyQueue(Queue* queue) {
    Node* firstNode = queue->first;
    destroyList(firstNode);
    free(queue);
}