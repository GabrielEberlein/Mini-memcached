#ifndef __PARSER_H
#define __PARSER_H 1

#include "common.h"




//! @brief Parser de texto.
//!
//! @param[in] buf - const char *.  No debe contener el '\n'
//! @param[out] toks - char *: arreglo de tokens
//! @param[out] lens - arreglo de enteros, contiene la longitud de los tokens
//! @param[out] ntok - cantidad de tokens

// writen : int, const void*, n -> ssize_t
/*
    Escribe todos los caracteres de una cadena en un socket
    Incluso ante interrumpciones 
*/
ssize_t writen(int fd, const void *buffer, size_t n);

// text_parser : char*, char*, int[] -> int
/*
    Separa un strings en tres, a partir de los espacios
*/
int text_parser(char *buf, char *toks[3], unsigned int lens[3] );

#endif
