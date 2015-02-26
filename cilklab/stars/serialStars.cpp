#include "serialStars.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits>

ppair serial_friendly_stars(point *inp, int N) {
  float min_dist = std::numeric_limits<float>::infinity();
  ppair closest_pair;

  cilk_for (int i=0; i<N; i++) {
    cilk_for (int j=i+1; j<N; j++) {
      point p = inp[i];
      point q = inp[j];
      float d = p.dist_squared(q);
      if (d < min_dist) {
        min_dist = d;
        closest_pair = ppair(p, q);
      }
    }
  }

  return closest_pair;
}

float closestUtil(Point Px[], Point Py[], int n)
{
    // If there are 2 or 3 points, then use brute force
    if (n <= 3)
        return bruteForce(Px, n);
    
    // Find the middle point
    int mid = n/2;
    Point midPoint = Px[mid];
    
    Point Pyl[mid+1];   // y sorted points on left of vertical line
    Point Pyr[n-mid-1];  // y sorted points on right of vertical line
    int li = 0, ri = 0;  // indexes of left and right subarrays
    for (int i = 0; i < n; i++)
    {
        if (Py[i].x <= midPoint.x)
            Pyl[li++] = Py[i];
        else
            Pyr[ri++] = Py[i];
    }
    
    // Consider the vertical line passing through the middle point
    // calculate the smallest distance dl on left of middle point and
    // dr on right side
    float dl = closestUtil(Px, Pyl, mid);
    float dr = closestUtil(Px + mid, Pyr, n-mid);
    
    // Find the smaller of two distances
    float d = min(dl, dr);
    
    // Build an array strip[] that contains points close (closer than d)
    // to the line passing through the middle point
    Point strip[n];
    int j = 0;
    for (int i = 0; i < n; i++)
        if (abs(Py[i].x - midPoint.x) < d)
            strip[j] = Py[i], j++;
    
    // Find the closest points in strip.  Return the minimum of d and closest
    // distance is strip[]
    return min(d, stripClosest(strip, j, d) );
}