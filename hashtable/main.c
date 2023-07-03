#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "./io.h"
#include "./hash.h"
#include "./bst.h"

void handle_command(char* command, HashTable table){
    char *request = strtok(command, " ");

    if(strcmp(request,"PUT") == 0){
        char *key = strtok(NULL," ");
        char *val = strtok(NULL," ");
        insert_hashtable(table, key, atoi(val));
    }

    if(strcmp(request,"GET") == 0){
        char* key = strtok(NULL," ");
        printf("%d\n",search_hashtable(table, key));
    }

    if(strcmp(request,"DEL") == 0){
        char* key = strtok(NULL," ");
        delete_hashtable(table, key);
    } 
}

int main(int argc, char *argv[]){
    if(argc != 2){
        printf("Invalid amount of arguments\n");
        return -1;
    }
    int commandsAmnt = 0;
    
    char **commands = readfile(argv[1], &commandsAmnt);
    HashTable table = hashtable_create(8);
    
    for(int i=0; i<commandsAmnt; i++){
        handle_command(commands[i], table);
    }
    printf("elems: %d | collisions: %d | cap: %d \n", table->numElems, table->collisions, table->capacity);
    // for(int i=0;i<table->capacity;i++){
    //     Node elem = table->elems[i];
    //     printf("%s %d\n", elem->key, elem->value);
    // }
    return 0;
}