// This code is part of the Problem Based Benchmark Suite (PBBS)
// Copyright (c) 2010 Guy Blelloch and Harsha Vardhan Simhadri and the PBBS team
//

#include "quickSort.h"
#include <stdlib.h>
#include <iostream>
#include <math.h>
//#include "CycleTimer.h"

int search(double* S, int n, double v) {
  for (int i=0; i < n; i++)
    if (v < S[i]) return i;
  return n;
}

// Binary Search
int binSearch(double* S, int n, double v) {
  double* T = S;
  while (n > 10) {
    int mid = n/2;
    if (v < T[mid]) n = mid;
    else {
      n = (n-mid)-1;
      T = T + mid + 1;
    }
  }
  return T-S+search(T,n,v);
}

// First step of counting sort.
// Counts how many times each integer in range 0..m-1
// appears in the array Index, which has length n.
// It side effects the counts array.
void genCounts(int *Index, int *counts, int m, int n) {
  for (int i = 0; i < m; i++)
    counts[i] = 0;
  for (int j = 0; j < n; j++)
    counts[Index[j]]++;
}

// Second step of counting sort.
// Copies the elements of A into the appropriate bucket in B.
// The Index array specifies for each key in A what bucket it belongs to.
// The offsets array has the offset of each bucket.
void relocate(double* A, double* B, int *Index, int *offsets, int n) {
  for (long j = 0; j < n; j++)
    B[offsets[Index[j]]++] = A[j];
}

#define SSORT_THR 1

void sampleSort (double* A, int n) {
  if (n < SSORT_THR) quickSortSerial(A, n);
  else {
    int isqrt = (int) sqrt((double) n);
    int numBlocks = isqrt/10;
    int numBuckets = isqrt/10;
    int blockSize = (n-1)/numBlocks + 1;
    double* pivots = new double[numBuckets-1];

    // Generate numBuckets-1 random pivots
    for (int j=0; j< numBuckets-1; ++j)
      pivots[j] = A[rand()%n];

    // Sort the pivots
    quickSortSerial(pivots, numBuckets-1);

    // Using binary search determine for each key which bucket it goes into
    int* bucketId = new int[n];
    cilk_for(int i = 0; i < n; i++)
      bucketId[i] = binSearch(pivots,numBuckets-1,A[i]);
    delete[] pivots;

    // Partition the input sequence into numBlocks parts and 
    // in parallel across the blocks count sequentially how many keys 
    // that block has in each bucket.
    int* counts = new int[numBlocks*numBuckets];
    cilk_for (int i2 = 0; i2 < numBlocks; i2++) {
      int offset = i2 * blockSize;
      int size =  (i2 < numBlocks - 1) ? blockSize : n - offset;
      genCounts(bucketId + offset, counts + i2 * numBuckets, numBuckets, size);
    }

    // In parallel for each bucket, we loop through the blocks (sequentially)
    // to calculate the total number of keys in each buckets.
    int* bucketStarts = new int[numBuckets+1];
    cilk_for (int i = 0; i < numBuckets; i++) {
      int sum = 0;
      for (int j = 0; j < numBlocks; j++)
      	sum += counts[j*numBuckets+i];
      bucketStarts[i] = sum;
    }
    // at this point bucketStarts contains a count of the total 
    // number of keys in each bucket

    // Now we calculate the offset in the target for each bucket
    // This is basicall a plus scan on bucketStarts
    int sum = 0;
    for (int i = 0; i < numBuckets; i++) {
      int v = bucketStarts[i];
      bucketStarts[i] = sum;
      sum += v;
    }
    

    // Now we calculate the offset in the target for each block
    // and each bucket.   Basically each block owns its own portion
    // of the bucket, and counts will indicate the start of that portion.
    cilk_for (int i = 0; i < numBuckets; i++) {
      int sum = bucketStarts[i];
      for (int j = 0; j < numBlocks; j++) {
      	int v = counts[j*numBuckets+i];
      	counts[j*numBuckets+i] = sum;
      	sum += v;
      }
    }
    bucketStarts[numBuckets] = n;

    // Now move everything to its final location in the buckets
    // The output B will be sorted by bucket, but not within the buckets
    double* B = new double[n];
    cilk_for (int i3 = 0; i3 < numBlocks; i3++) {
      int offset = i3 * blockSize;
      int size =  (i3 < numBlocks - 1) ? blockSize : n - offset;
      relocate(A + offset, B, bucketId + offset,
         counts + i3 * numBuckets,
         size);
    }

    // Finish by sorting within each bucket
    cilk_for (int r = 0; r < numBuckets; r++) {
      int size = bucketStarts[r+1] - bucketStarts[r];
      for (int i=bucketStarts[r]; i < bucketStarts[r+1]; i++)
        A[i] = B[i];
      quickSortSerial(A + bucketStarts[r], size);
    }

    delete[] counts;
    delete[] bucketId;
    delete[] B;
    delete[] bucketStarts;
  }
}
