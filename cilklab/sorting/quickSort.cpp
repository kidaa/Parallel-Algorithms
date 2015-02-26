// This code is part of the Problem Based Benchmark Suite (PBBS)
// Copyright (c) 2010 Guy Blelloch and the PBBS team

// Needed for defining e.g. pair<>
#include <algorithm>

void insertionSort(double *A, int n) {
  for (int i=0; i<n; i++) {
    double v = A[i];
    double *B = A + i;
    while (--B >= A && (v < *B)) {
      *(B+1) = *B;
    }
    *(B+1) = v;
  }
}

double medianOf3(double a, double b, double c) {
  return (a < b) ? ((b < c) ? b : ((a < c) ? c : a))
                 : ((a < c) ? a : ((b < c) ? c : b));
}

std::pair<double*,double*> split(double *A, int n) {
  double p = medianOf3(A[n/4], A[n/2], A[(3*n)/4]);
  double *L = A;      // below L are less than pivot
  double *M = A;      // between L and M are equal to pivot
  double *R = A+n-1;  // above R are greater than pivot

  while (true) {
    while (!(p < *M)) {
      if (*M < p) std::swap(*M,*(L++));
      if (M >= R) break;
      M++;
    }
    while (p < *R) R--;
    if (M >= R) break;
    std::swap(*M,*R--);
    if (*M < p) std::swap(*M,*(L++));
    M++;
  }

  return std::pair<double*,double*>(L,M);
}

#define ISORT_THRESHOLD 25

void quickSortSerial(double *A, int n) {
  if (n < ISORT_THRESHOLD) {
    insertionSort(A, n);
  }
  else {
    std::pair<double*,double*> X = split(A, n);
    quickSortSerial(A, X.first-A);
    quickSortSerial(X.second, A+n-X.second);
  }
}

#define SPAWN_THRESHOLD 1000

void quickSort(double *A, int n) {
  if (n < SPAWN_THRESHOLD) {
    quickSortSerial(A, n);
  }
  else {
    std::pair<double*,double*> X = split(A, n);
    cilk_spawn quickSort(A, X.first-A);
    quickSort(X.second, A+n-X.second);
    cilk_sync;
  }
}
