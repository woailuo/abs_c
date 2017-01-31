#include<stdio.h>
#include<stdlib.h>

int main(int argc, char *argv[]){

  int *p = (int*)malloc(sizeof(int));
  int *q = (int*)malloc(sizeof(int));
  return 0;
  int n, m ;
  scanf("%d, %d", &n, &m);

  if(n > 10){
    printf(" input num is greater than 10 \n");

    if(p) { free(p); p = NULL;     printf(" Pointer p has been deallocated \n");}

    if(q)  { free(q); q = NULL;     printf(" Pointer q has been deallocated \n");}

    return 0;
  }

  if(p)
    *p = n;

  if(q)
    *q = m;

    printf(" n + m = %d\n", *p + *q);

    if(p) { free(p); p = NULL;     printf(" Pointer p has been deallocated \n");}

    if(q)  { free(q); q = NULL;     printf(" Pointer q has been deallocated \n");}

  return 0;
}
