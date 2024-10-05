#include <stdio.h>
#include <stdlib.h>

/*
Compile and run with (pedantic):

gcc -Wall -Wpedantic -o /tmp/tmp radix.c && /tmp/tmp

*/

int digit_of_char(char c) {
  if (c >= '0' && c <= '9') {
    return c - '0';
  } else if (c >= 'A' && c <= 'Z') {
    return c - 'A' + 10;
  } else {
    fprintf(stderr, "digit_of_char: %c\n", c);
    exit(EXIT_FAILURE);
  }
}

int main() {
  int acc = 0;
  char arr[] = {'A', 'B', 'C'};

  int length = sizeof(arr) / sizeof(arr[0]);

  for (int i = 0; i < length; i++) {
    acc = (digit_of_char(arr[i])) + 16 * acc;
  }

  printf("%d\n", acc);
  return 0;
}
