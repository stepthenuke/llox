#include <stdio.h>

int read_int() {
   int res;
   scanf("%d", &res);
   return res;
}

double read_double() {
   double res;
   scanf("%lf", &res);
   return res;
}

void print_int(int v) {
   printf("%d\n", v);
}

void print_double(double v) {
   printf("%lf\n", v);
}
