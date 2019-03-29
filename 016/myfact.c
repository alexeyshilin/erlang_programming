#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>


long factorial_recursion(int);
long factorial_function(int);

int main(void) {
	char *line = NULL;
	size_t len = 0;
	ssize_t read = 0;
	while (read != -1) {
		//puts("enter a line");
		read = getline(&line, &len, stdin);
		//printf("line = %s", line);
		//printf("line length = %zu\n", read);

		int i = atoi(line);
		int res = factorial_recursion(i);

		printf("result = %d", res);

		puts("");
		break;
	}
	free(line);
	return 0;
}

long factorial_recursion(int n)
{
  if (n == 0)
    return 1;
  else
    return(n * factorial_recursion(n-1));
}

long factorial_function(int n)
{
  int c;
  long result = 1;
 
  for (c = 1; c <= n; c++)
    result = result * c;
 
  return result;
}

// gcc -o myfact myfact.c
