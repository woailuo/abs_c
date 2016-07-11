#include <stdio.h>
#include <stdlib.h>
//#include "b.h"

char * pstring(int num);
extern int a;
int main(int argc, char * argv[]){
  cnum();
  cnum();
  cnum();
  onum();
  int num = a;
  char * s = pstring(num);
  printf("%s\n", s);
  return 0;
}

char * pstring(int num){
  printf("this is a funciton pstring: %d\n", num);
  return  "success";
}
