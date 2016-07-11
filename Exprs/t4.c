#include <stdio.h>
#include <stdlib.h>

int main(){
  int *p = (int *)malloc(sizeof(int));
  int x;
  scanf("%d", &x);

  if(x>10){
    free(p);
    printf(" x > 10, and pointer has deallocated \n");
    return 0 ;
  }
  else{
    printf(" x <= 10 \n");
  }

  printf(" x * 2 is %d \n", x * 2);

  free(p);
  return 0;
}
