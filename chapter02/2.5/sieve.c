#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/*
clang-format off

echo ./sieve.c | entr -c bash -c "gcc -W'all' -W'extra' -W'pedantic' -W'error' -g -o /tmp/tmp.c.exe ./sieve.c -lm && echo OK"
gcc -W'all' -W'extra' -W'pedantic' -W'error' -g -o /tmp/tmp.c.exe ./sieve.c -lm && /tmp/tmp.c.exe

clang-format on
*/

int get_max() {
  printf("Enter a number: ");
  int max;
  scanf("%d", &max);
  return max;
}

void compute(int max, int *prime) {
  int limit = sqrt(max);
  prime[0] = 0;
  prime[1] = 0;
  for (int n = 2; n <= limit; n++) {
    if (prime[n]) {
      int m = n * n;
      while (m <= max) {
        prime[m] = 0;
        m += n;
      }
    }
  }
}

int main() {
  int max = get_max();
  int *prime = malloc((max + 1) * sizeof(int));
  for (int i = 0; i <= max; i++) {
    prime[i] = 1;
  }

  compute(max, prime);

  for (int n = 2; n <= max; n++) {
    if (prime[n]) {
      printf("%d\n", n);
    }
  }

  free(prime);
  return 0;
}
