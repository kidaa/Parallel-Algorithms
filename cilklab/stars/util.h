#pragma once

#include <stdio.h>

struct point {
  float x;
  float y;

  point(float _x, float _y) : x(_x), y(_y) {}
  point() : x(0.0), y(0.0) {}

  // Squares preserve order, so don't take the sqrt for
  // better performance.
  float dist_squared(point p2) {
    float xs = (p2.x-x) * (p2.x-x);
    float ys = (p2.y-y) * (p2.y-y);
    return xs+ys;
  }

  void print() {
    printf("(%.0f, %.0f)", x, y);
  }

  bool operator==(point p2) {
    return x == p2.x && y == p2.y;
  }
};

struct ppair {
  point p1;
  point p2;

  ppair(point _p1, point _p2) : p1(_p1), p2(_p2) {}
  ppair() : p1(point()), p2(point()) {}

  bool operator==(ppair p) {
    return p1.dist_squared(p2) == p.p1.dist_squared(p.p2);
  }

  void print() {
    printf("(");
    p1.print();
    printf(", ");
    p2.print();
    printf(")");
  }
};
