#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

#include <pthread.h>

Queue queue_create() {
    Queue queue = malloc(sizeof(struct _Queue));
    queue->first = NULL;
    queue->last = NULL;
    pthread_mutex_init(&(queue->lock), NULL);
    return queue;
}

void queue_push(Queue queue, Node node) {
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

void queue_pop(Queue queue) {
    pthread_mutex_lock(&(queue->lock));
    if (queue->first != NULL) {
        Node secondNode = queue->first->prev;
        queue->first = secondNode;
    }
    pthread_mutex_unlock(&(queue->lock));
}

void queue_relocate(Queue queue, Node node) {
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

void queue_delete(Queue queue, Node node){
    pthread_mutex_lock(&(queue->lock));
    if(node->prev) node->prev->next = node->next; 
    else queue->last = node->next;
    if(node->next) node->next->prev = node->prev;
    else queue->first = node->prev;
    node_destroy(node);
    pthread_mutex_unlock(&(queue->lock));
}

void list_destroy(Node firstNode) {
    Node currentNode = firstNode;
    while (currentNode != NULL) {
        Node nextNode = currentNode->next;
        free(currentNode);
        currentNode = nextNode;
    }
}

void queue_destroy(Queue queue) {
    Node firstNode = queue->first;
    list_destroy(firstNode);
    pthread_mutex_destroy(&(queue->lock));
    free(queue);
}