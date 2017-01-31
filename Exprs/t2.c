#include <stdio.h>
#include <stdlib.h>

struct Database{
 int** addre;
};
struct Connection{
    struct Database *db;
  int *age;
  int year;
};

int fun (int n){

  if (n<=1)
    {
      return 1;
    }
  else {
    int *p =(int*)malloc(sizeof(int));
    free(p);
    return n + fun(n-1);
  }
}

int func()
{

  int **q;
  int n = 1 + 5;

 struct Connection *conn = malloc(sizeof(struct Connection));

 int *p = (int*)malloc(sizeof(int));

 int m = 1;

 int c ;
 scanf("%d",&c);

 conn->db =malloc(sizeof(struct Database));

  if(conn)
    {
      if(conn->db) free(conn->db);

      m = m+ 2;

      free(conn);

    }

  return 0;


}

int main()
{
  int uio ;
  scanf("%d",&uio);
  int *p = (int*)malloc(sizeof(int));
  switch (uio) {
  case 1 : if(uio){free(p);}printf(" 1 \n");break;
  case 2 :printf("  2 \n");int m = 3 ; m--; break;
  case 3 : printf(" 3 \n");free(p);break;
  default: ;break;
  }

  return 0;
}
