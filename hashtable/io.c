#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include "io.h"

static inline void quit(const char *s)
{
	perror(s);
	exit(1);
}

char** readfile(const char *path, int *len)
{
	FILE *f = fopen(path, "rb");
	if (f == NULL) quit("readfile.fopen");

	int i=0, commandSize = 64, commandsAmnt = 1024;
	char buff[64];
	char c = getc(f);
	char **commands = malloc(sizeof(char*) * commandsAmnt);

	while (c != EOF) {
		if (commandsAmnt <= (*len)) {
			commandsAmnt *= 2;
			char **aux = realloc(commands, sizeof(char*) * commandsAmnt);
			if(aux != NULL) commands = aux;
		}
		commands[(*len)] = malloc(sizeof(char) * commandSize);
		for (i=0; c != '\n' && c != EOF; i++){
			commands[(*len)][i] = c;
			c = getc(f);
		}
		commands[(*len)][i] = '\0';
		c = getc(f);
		(*len)++;
	}

	fclose(f);

	return commands;
}

void free_array(char **array, unsigned size){
	for(unsigned i=0; i < size; i++){
		free(array[i]);
		array[i] = NULL;
	}
	free(array);
}