#include <stdio.h>
#include <stdlib.h>

int main(){
  int *p = (int *)malloc(sizeof(int)) ;
  int a = 4, b = 5;
  int c = sum(a,b);
  return 1;
  printf("the c is %d\n", c);
  free(p);
  return 0;
}

int sum(int a,int b){
  int *p = (int *)malloc(sizeof(int));
  int c = a + b ;
  if(c < 10) {
    free(p);
    p = NULL;
    return c;
  }
  sub(a, b); 
  if(p) free(p);
  return c;
}

int sub(int a, int b){
  int *p = (int *)malloc(sizeof(int));
  int c = a - b;
  if (c > -5){
      return c;
  }
  sum(a,b) ;
  free(p);
  return c ;
}
