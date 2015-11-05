/*
 * Straightforward sequential implementation of American Put-Option
 * pricing.  Written naively, as someone who is just trying to write
 * simple and not obviously-inefficient C.  If the compiler can
 * vectorise it, great.
 *
 * I have scattered some performance notes around the program.
 *
 * Written by Troels Henriksen (athas@sigkill.dk).
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>

typedef float real;
#define powr powf
#define sqrtr sqrtf

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
  unsigned int resolution=1000000;
  long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
  result->tv_sec = diff / resolution;
  result->tv_usec = diff % resolution;
  return (diff<0);
}

static const int strike = 100;
static const int bankDays = 252;
static const int s0 = 100;
static const real r = 0.03;
static const real alpha = 0.07;
static const real sigma = 0.20;

#define MAX(x,y) ((x) < (y) ? (y) : (x))

real binom(int expiry) {
  int n = expiry * bankDays;
  real dt = ((real)expiry) / n;
  real u = exp(alpha*dt+sigma*sqrtr(dt));
  real d = exp(alpha*dt-sigma*sqrtr(dt));
  real stepR = exp(r*dt);
  real q = (stepR-d)/(u-d);
  real qUR = q/stepR;
  real qDR = (1-q)/stepR;

  /* uPow and dPow could be inlined, but it is much more efficient to
     cache the expensive calls to the power function.
   */
  real *uPow = malloc(sizeof(real) * (n+1));
  for (int j = 0; j < n+1; j++) {
    uPow[j] = powr(u, j);
  }

  real *dPow = malloc(sizeof(real) * (n+1));
  for (int j = 0; j < n+1; j++) {
    dPow[j] = powr(d, n-j);
  }

  /* Computation of st has been folded into the following two loops.
   */
  real *put = malloc(sizeof(real) * (n+1));
  for (int j = 0; j < n+1; j++) {
    real st_j = s0 * (uPow[j] * dPow[j]);
    real x = strike - st_j;
    put[j] = MAX(x,0);
  }

  /* This loop updates put in-place, but it is safe, as we only depend
     on the elements of put that are at and after the element that we
     are currently updating.  To turn this into a parallel loop, we
     would have to double-buffer put. */
  for (int i = n; i > 0; i--) {
    for (int j = 0; j < i; j++) {
      real st_j = s0 * (uPow[j] * dPow[n+1-i+j]);
      real x = strike - st_j;
      real y = qUR * put[j+1] + qDR * put[j];
      put[j] = MAX(x,y);
    }
  }

  real result = put[0];

  free(uPow);
  free(dPow);
  free(put);

  return result;
}

int main() {
  int expiry;

  scanf("%d", &expiry);

  unsigned long int elapsed_usec = 0;

  struct timeval t_start, t_end, t_diff;
  gettimeofday(&t_start, NULL);

  real result = binom(expiry);

  gettimeofday(&t_end, NULL);
  timeval_subtract(&t_diff, &t_end, &t_start);
  elapsed_usec = t_diff.tv_sec*1e6+t_diff.tv_usec;

  FILE* runtime_file = fopen(getenv("HIPERMARK_RUNTIME"), "w");
  FILE* result_file = fopen(getenv("HIPERMARK_RESULT"), "w");

  fprintf(runtime_file, "%ld\n", elapsed_usec / 1000);
  fprintf(result_file, "%f\n", result);

  fclose(runtime_file);
  fclose(result_file);

  return 0;
}
