#include <stdlib.h>
#include <stdio.h>

int main(){
  int *n = (int *)malloc(sizeof(int));
  int **p = (int **)malloc(sizeof(int)) ;

  int lconst_n = 1;
  if(lconst_n){
    
  if(n){
    int *x = (int*)malloc(sizeof(int));
    *x = 10;
    *p = x;
    printf("x has been allocated %d, and **p is %d \n", *x, **p);
  }
  else {
    printf("m is a null pointer \n");
  }

  if(n){
    free(*p);
    *p = NULL;
    printf("x has been deallocated \n");
  }
  else {
    printf("m is a null pointer \n");
  }

  }

  free(n);
  free(p);
    
  return 0;
}
