#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <getopt.h>
#include <vector>
#include <cilk/cilk_api.h>

#include "CycleTimer.h"
#include "util.h"
#include "serialStars.h"
#include "cilkStars.h"

#define MIN_DELTA 20

float rand_coord(int max) {
  return (float)(rand() % max + 1);
}

bool is_close(point i1, point i2) {
  return abs(i1.x - i2.x) < MIN_DELTA &&
         abs(i1.y - i2.y) < MIN_DELTA;
}

bool is_existing_test(point p, std::vector<point> existing) {
  std::vector<point>::iterator it;
  for (it = existing.begin(); it != existing.end(); ++it) {
    point e = *it;
    if (is_close(e, p)) {
      return true;
    }
  }
  return false;
}

void fill_test_cases(point *inp, int N) {
  srand(time(NULL));
  int max = N*10;

  std::vector<point> existing;

  int i = 0;
  while (i < N) {
    point p = point(rand_coord(max), rand_coord(max));
    if (!is_existing_test(p, existing)) {
      inp[i] = p;
      existing.push_back(p);
      i++;
    }
  }
}

void print_time(const char *desc, double start, double end) {
  printf("%s:\t%.3fs", desc, end-start);
}

int main(int argc, char **argv) {
  // default size
  int N = 50 * 1000;

  // check optional arguments
  char c;
  while ((c = getopt(argc, argv, "t:n:h")) != -1) {
    switch (c) {
      case 't':
        __cilkrts_set_param("nworkers", optarg);
        break;
      case 'n':
        N = atoi(optarg);
        break;
      case 'h':
        printf("Usage: %s [-t <threads>] [-n <size>] [-h]\n", argv[0]);
        exit(0);
      default:
        exit(1);
    }
  }

  int nthreads = __cilkrts_get_nworkers();
  double start, end;

  printf("~ Generating %d test cases\n", N);

  point inp[N];
  fill_test_cases(inp, N);

  printf("~ Beginning tests with %d elements on %d threads\n", N, nthreads);

  printf("~ Running serial friendly stars\n");
  start = CycleTimer::currentSeconds();
  ppair p1 = serial_friendly_stars(inp, N);
  end = CycleTimer::currentSeconds();
  double serial_time = end-start;

  printf("Serial result:\t");
  p1.print();
  printf("\n");
  print_time("Serial time", start, end);
  printf("\n\n");

  ppair p2;
  // get the cores started and "warmed up"
  for (int i=0; i<10; i++) {
    p2 = cilk_friendly_stars(inp, std::min(40000,N));
  }

  printf("~ Running Cilk friendly stars\n");
  start = CycleTimer::currentSeconds();
  p2 = cilk_friendly_stars(inp, N);
  end = CycleTimer::currentSeconds();
  double par_time = end-start;

  if (p1 == p2) {
    printf("Cilk result matches serial result!\n");
  }
  else {
    printf("Cilk result is INCORRECT!\n");
    printf("Cilk result:\t");
    p2.print();
    printf("\n");
  }

  print_time("Cilk time", start, end);
  printf("\t[Speedup: %.2fx]\n", serial_time/par_time);

  return 0;
}
