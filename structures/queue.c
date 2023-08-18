#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

#include <pthread.h>

Queue* create_queue() {
    Queue* queue = (Queue*)malloc(sizeof(Queue));
    queue->first = NULL;
    queue->last = NULL;
    pthread_mutex_init(&(queue->lock), NULL);
    return queue;
}

void push_queue(Queue* queue, Node node) {
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

int pop_queue(Queue* queue) {
    pthread_mutex_lock(&(queue->lock));
    if (queue->first == NULL)  return -1;
    Node secondNode = queue->first->prev;
    queue->first = secondNode;
    pthread_mutex_unlock(&(queue->lock));
    return 0;
}

void remove_queue(Queue* queue, Node node){
    pthread_mutex_lock(&(queue->lock));
    if(node->prev) node->prev->next = node->next; 
    else queue->last = node->next;

    if(node->next) node->next->prev = node->prev;
    else queue->first = node->prev;
    pthread_mutex_unlock(&(queue->lock));
}

void relocate_queue(Queue* queue, Node node) {
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
    destroy_list(firstNode);
    pthread_mutex_destroy(&(queue->lock));
    free(queue);
}