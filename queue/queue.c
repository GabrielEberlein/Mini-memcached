#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

Queue* create_queue() {
    Queue* queue = (Queue*)malloc(sizeof(Queue));
    queue->first = NULL;
    queue->last = NULL;
    return queue;
}

Node create_node(int data) {
    Node newNode = (Node)malloc(sizeof(Node));
    newNode->next = NULL;
    newNode->prev = NULL;
    newNode->data = data;
    return newNode;
}

void push_queue(Queue* queue, int data) {
    Node newNode = create_node(data);
    if (queue->first == NULL) {
        queue->first = newNode;
        queue->last = newNode;
    } else {
        newNode->prev = queue->last;
        queue->last->next = newNode;
        queue->last = newNode;
    }
}

int pop_queue(Queue* queue) {
    if (queue->first == NULL)  return -1;
    Node lastNode = queue->last;
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

int relocate_queue(Queue* queue, Node node) {
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

void destroy_list(Node firstNode) {
    Node currentNode = firstNode;
    while (currentNode != NULL) {
        Node nextNode = currentNode->next;
        free(currentNode);
        currentNode = nextNode;
    }
}

void destroy_queue(Queue* queue) {
    Node firstNode = queue->first;
    destroyList(firstNode);
    free(queue);
}