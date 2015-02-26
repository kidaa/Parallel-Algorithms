#include "cilkStars.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits>

int main(){
    int sum = 0;
    int i = 0;
    cilk_for (i = 0; i <= 10000; i++)
    sum += i;
    printf("%d\n",sum);
}
