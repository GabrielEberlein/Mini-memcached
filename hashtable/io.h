#ifndef __IO_H_
#define __IO_H_

/*
 * Lee un archivo y devuelve sus contenidos en un buffer obtenido con
 * malloc(). Debe ser liberado luego. En [*len] se guarda la longitud de
 * todo lo leído.
 */
char** readfile(const char *path, int *len);

void free_array(char **dictionary, unsigned size);

#endif
