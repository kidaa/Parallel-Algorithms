#include <iostream>
#include <algorithm>
#include "CycleTimer.h"
#include <cilk/cilk_api.h>
#include <getopt.h>

using namespace std;

#define ROUNDS_REPEAT 3

void serialSort(double *A, int n);
void parallelSort(double *A, int n);

void print_time(const char *desc, double start, double end) {
  printf("%s:\t%.3fs", desc, end-start);
}

int main(int argc, char* argv[]) {
  // default size
  int n = 100 * 1000 * 1000;

  // check optional arguments
  char c;
  while ((c = getopt(argc, argv, "t:n:h")) != -1) {
    switch (c) {
      case 't':
        __cilkrts_set_param("nworkers", optarg);
        break;
      case 'n':
        n = atoi(optarg);
        break;
      case 'h':
        printf("Usage: %s [-t <threads>] [-n <size>] [-h]\n", argv[0]);
        exit(0);
      default:
        exit(1);
    }
  }

  int nthreads = __cilkrts_get_nworkers();

  printf("~ Sorting %d elements on %d threads\n", n, nthreads);
  printf("\n");

  double *A = new double[n];
  for (int i = 0; i < n; i++) {
    A[i] = rand() % n;
  }

  double *B = new double[n];
  double start, end;
  double serial_time, par_time;

  for (int i=0; i<ROUNDS_REPEAT; i++) {
    cilk_for (int i=0; i<n; i++)
      B[i] = A[i];

    start = CycleTimer::currentSeconds();
    serialSort(B, n);
    end = CycleTimer::currentSeconds();
    serial_time = end - start;
    print_time("Serial time", start, end);
    printf("\n");

    cilk_for (int i=0; i<n; i++)
      B[i] = A[i];

    start = CycleTimer::currentSeconds();
    parallelSort(B, n);
    end = CycleTimer::currentSeconds();
    par_time = end - start;
    print_time("Parallel time", start, end);

    printf("\t[Speedup: %.2fx]\n", serial_time/par_time);
    printf("\n");
  }
}
