#include "cilkStars.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits>
#include <algorithm>

// Used when there are only a small number of points.
// In such cases, overhead time is longer than calculation
// Using sequential algorithm is more efficient
ppair bruteForce(point *p, int n) {
    float min = std::numeric_limits<float>::infinity();
    point p1;
    point p2;
    float dist;
    for (int i = 0; i < n; ++i)
    {
        for (int j = i+1; j < n; ++j)
        {
            dist = p[i].dist_squared(p[j]);
            if (dist < min)
            {
                p1 = p[i];
                p2 = p[j];
                min = dist;
            }
        }
    }
    return ppair(p1, p2);
}

// Compare two pairs, return one with shorter distance
ppair minDistPair(ppair pair1, ppair pair2) {
    float dist1 = pair1.p1.dist_squared(pair1.p2);
    float dist2 = pair2.p1.dist_squared(pair2.p2);
    if (dist1 == 0) {
        return pair2;
    } else if (dist2 == 0) {
        return pair1;
    } else if (dist1 <= dist2) {
        return pair1;
    } else {
        return pair2;
    }
}

// Find shortest distance between two points that are crossing the middle line
ppair stripClosest(point strip[], int size, float d) {
    point point1;
    point point2;
    float min = d;
    float dist;
    // For any p in strip, there is at most 6 points to check
    // This is because we already have that any pair of points on one side have
    // a distance less than d.
    // Hence, inner loop is actually of this part is of O(n) work instead of O(n^2)
    for (int i = 0; i < size; ++i) {
        // This loop would break by condition "(strip[j].y - strip[i].y) < min"
        // in at most 6 iters
        for (int j = i+1; j < size && (strip[j].y - strip[i].y) < min; j++) {
            dist = strip[i].dist_squared(strip[j]);
            if (dist < min) {
                min = dist;
                point1 = strip[i];
                point2 = strip[j];
            }
        }
    }
    return ppair(point1, point2);
}

// Recursive function. Find a vertical line that passes the middle point,
// divide all points into two halves, find shortest-dist pairs on each side,
// find the shortest-dist pair that crosses the line, return shortest
// among the 3
ppair closestUtil(point *px, point *py, int n) {
    ppair pair;
    // If there are only a few points, use brute force
    if (n <= 3)
    {
        return bruteForce(px, n);
    }
    
    int mid = n/2;
    point midPoint = px[mid];
    
    // Divide points in y-sorted array, keep order
    point pyl[mid+1];    // y sorted points on left of vertical line
    point pyr[n-mid-1];  // y sorted points on right of vertical line
    int li = 0, ri = 0;
    for (int i = 0; i < n; i++)
    {
        if (py[i].x <= midPoint.x) {
            pyl[li] = py[i];
            li ++;
        } else {
            pyr[ri] = py[i];
            ri ++;
        }
    }
    
    // cilk applied here, spwan & sync to avoid race
    ppair pair1 = cilk_spawn closestUtil(px, pyl, mid);
    ppair pair2 = closestUtil(px + mid, pyr, n - mid);
    cilk_sync;
    pair = minDistPair(pair1, pair2);
    float d = pair.p1.dist_squared(pair.p2);
    
    // Build an array strip[] that contains points close (closer than d)
    // to the line passing through the middle point
    point strip[n];
    int j = 0;
    for (int i = 0; i < n; i++) {
        if (abs(py[i].x - midPoint.x) < d) {
            strip[j] = py[i], j++;
        }
    }
    // Find the closest points in strip
    pair = minDistPair(pair, stripClosest(strip, j, d));
    // printPair(pair);
    return pair;
}

// The following functions are for quicksort based on x coordinate of x
// Revised from given sorting code
point medianOf3X(point a, point b, point c) {
    return (a.x < b.x) ? ((b.x < c.x) ? b : ((a.x < c.x) ? c : a))
    : ((a.x < c.x) ? a : ((b.x < c.x) ? c : b));
}

point medianOf3Y(point a, point b, point c) {
    return (a.y < b.y) ? ((b.y < c.y) ? b : ((a.y < c.y) ? c : a))
    : ((a.y < c.y) ? a : ((b.y < c.y) ? c : b));
}

std::pair<point*,point*> splitX(point *A, int n) {
    point p = medianOf3X(A[n/4], A[n/2], A[(3*n)/4]);
    point *L = A;      // below L are less than pivot
    point *M = A;      // between L and M are equal to pivot
    point *R = A+n-1;  // above R are greater than pivot
    
    while (true) {
        while (!(p.x < (*M).x)) {
            if ((*M).x < p.x) std::swap(*M,*(L++));
            if (M >= R) break;
            M++;
        }
        while (p.x < (*R).x) R--;
        if (M >= R) break;
        std::swap(*M,*R--);
        if ((*M).x < p.x) std::swap(*M,*(L++));
        M++;
    }
    
    return std::pair<point*,point*>(L,M);
}

std::pair<point*,point*> splitY(point *A, int n) {
    point p = medianOf3Y(A[n/4], A[n/2], A[(3*n)/4]);
    point *L = A;      // below L are less than pivot
    point *M = A;      // between L and M are equal to pivot
    point *R = A+n-1;  // above R are greater than pivot
    
    while (true) {
        while (!(p.y < (*M).y)) {
            if ((*M).y < p.y) std::swap(*M,*(L++));
            if (M >= R) break;
            M++;
        }
        while (p.y < (*R).y) R--;
        if (M >= R) break;
        std::swap(*M,*R--);
        if ((*M).y < p.y) std::swap(*M,*(L++));
        M++;
    }
    
    return std::pair<point*,point*>(L,M);
}

// For sequential sorting when n is small
int compareX(const void* a, const void* b)
{
    point *p1 = (point *)a,  *p2 = (point *)b;
    if (p1->x - p2->x == 0) {
        return 0;
    } else if (p1->x - p2->x > 0) {
        return 1;
    } else {
        return -1;
    }
}

int compareY(const void* a, const void* b)
{
    point *p1 = (point *)a,  *p2 = (point *)b;
    if (p1->y - p2->y == 0) {
        return 0;
    } else if (p1->y - p2->y > 0) {
        return 1;
    } else {
        return -1;
    }
}

// Use sequential sorting when n is less than threshold
#define SPAWN_THRESHOLD 1000

void quickSortX(point *p, int n) {
    if (n < SPAWN_THRESHOLD) {
        qsort(p, n, sizeof(point), compareX);
    }
    else {
        std::pair<point*,point*> X = splitX(p, n);
        cilk_spawn quickSortX(p, X.first-p);
        quickSortX(X.second, p+n-X.second);
        cilk_sync;
    }
}

void quickSortY(point *p, int n) {
    if (n < SPAWN_THRESHOLD) {
        qsort(p, n, sizeof(point), compareY);
    }
    else {
        std::pair<point*,point*> X = splitY(p, n);
        cilk_spawn quickSortY(p, X.first-p);
        quickSortY(X.second, p+n-X.second);
        cilk_sync;
    }
}

ppair cilk_friendly_stars(point *inp, int n) {
    // Copy point array, O(n), O(n)
    point px[n];
    point py[n];
    for (int i = 0; i < n; i++)
    {
        px[i] = inp[i];
        py[i] = inp[i];
    }
    // parallel quick sort, O(nlogn), O(n)
    quickSortX(px, n);
    quickSortX(py, n);
    // Divide and conquor. In each iteration, divide all points into two
    // parts, recursively find the shortest-dist pair on each side, and find
    // the shortest-dist pair that has one point on each side. Compare the
    // 3 and get the shortest one.
    // This recursion has logn steps, in each step we need O(n) work to
    // divide and calculate distances. Therefore the work is O(nlogn)
    // By using cilk to find shortest pair in parallel, the span is O(n)
    return closestUtil(px, py, n);
}