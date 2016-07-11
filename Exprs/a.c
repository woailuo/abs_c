#include <stdio.h>
#include <stdlib.h>

int sum(int a, int b){
  return a + b;
}

int main(){
  int *p = (int*)malloc(sizeof(int));

  if(p != NULL){
    *p = 6;
    printf("*p is %d \n", *p);
    printf("available pointer p \n");
  }else{
    sum(2,4);
    printf(" p is a null pointer \n");
  }

  if(p != NULL){
    free(p);
    p =NULL;
    printf(" p has been deallocated \n");
  }else{
      printf(" p is a null pointer \n");
  }
  
  return 0;
}
