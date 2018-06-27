#include <R.h>
#include <Rinternals.h>

float mmap_rev_float(const float x, int len) {
  float ret;
  int i, j;
  unsigned char *x_ptr   = (unsigned char *)&x;
  unsigned char *ret_ptr = (unsigned char *)&ret;

  for(i=0, j=len-1; i<len; i++, j--) {
    ret_ptr[i] = x_ptr[j];
  }

  return ret;
}

double mmap_rev_double(const double x, int len) {
  double ret;
  int i, j;
  unsigned char *x_ptr   = (unsigned char *)&x;
  unsigned char *ret_ptr = (unsigned char *)&ret;

  for(i=0, j=len-1; i<len; i++, j--) {
    ret_ptr[i] = x_ptr[j];
  }

  return ret;
}

long mmap_rev_long(const long x, int len) {
  long ret;
  int i, j;

  unsigned char *x_ptr   = (unsigned char *)&x;
  unsigned char *ret_ptr = (unsigned char *)&ret;

  for(i=0, j=len-1; i<len; i++, j--) {
    ret_ptr[i] = x_ptr[j];
  }

  return ret;
}
short mmap_rev_short(const short x, int len) {
  short ret;
  int i, j;

  unsigned char *x_ptr   = (unsigned char *)&x;
  unsigned char *ret_ptr = (unsigned char *)&ret;

  for(i=0, j=len-1; i<len; i++, j--) {
    ret_ptr[i] = x_ptr[j];
  }

  return ret;
}

int mmap_rev_int(const int x, int len) {
  int ret;
  int i, j;

  unsigned char *x_ptr   = (unsigned char *)&x;
  unsigned char *ret_ptr = (unsigned char *)&ret;

  for(i=0, j=len-1; i<len; i++, j--) {
    ret_ptr[i] = x_ptr[j];
  }

  return ret;
}
