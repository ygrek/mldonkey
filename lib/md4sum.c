#include "md4.h"
#include <stdio.h>

#define BUFFER_LEN 8192

void main (int nargs, char *args[])
{
  char *filename  = args[1];
  unsigned char digest[16];
  FILE *file;
  MD4_CTX context;
  int len;
  unsigned char buffer[BUFFER_LEN];

  if ((file = fopen (filename, "rb")) == NULL){
    exit(2);
  }   else {
    int i;
    MD4Init (&context);
    while ((len = fread (buffer, 1, BUFFER_LEN, file))!=0){
      MD4Update (&context, buffer, len);}
    MD4Final (digest, &context);
    for(i=0; i<16; i++) printf("%d", digest[i]);
    printf("\n");


    fclose (file);
  }
}
