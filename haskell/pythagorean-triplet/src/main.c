#include <stdio.h>

unsigned int int_sqrt(unsigned int s) {
	if (s <= 1) 
		return s;

	unsigned int x0 = s / 2;
	unsigned int x1 = (x0 + s / x0) / 2;
    
  printf("%d %d\n", x0, x1);
	while (x1 < x0) {
		x0 = x1;
		x1 = (x0 + s / x0) / 2;
    printf("%d %d\n", x0, x1);
	}		
	return x0;
}

int main() {
  printf("%d\n", int_sqrt(8));
}
