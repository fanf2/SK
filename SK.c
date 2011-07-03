#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

typedef enum {
  primin,
  prim_I, prim_J, prim_K, prim_S, prim_Y,
  prim_number,
  prim_floor, prim_ceil, prim_abs, prim_neg,
  prim_add, prim_sub, prim_mul, prim_div, prim_mod, prim_pow,
  primax
} prim;

typedef union word {
  union word *ptr;
  prim prim;
  double num; /* only used in the second word of a cell */
} word, *cell;

word *heap_lo, *heap_ptr, *heap_hi;

size_t heap_size = 1<<10;

static inline bool primitive(word w) {
  return(primin < w.prim && w.prim < primax);
}

static cell cheney(cell root) {
  word *new = malloc(heap_size * sizeof(*new));

  word *ptr = new;
  ptr[0] = root[0];
  ptr[1] = root[1];
  ptr += 2;

  /* scan new heap word-by-word */
  for(word *here = new; here < ptr; here++) {
    if(primitive(*here)) {
      /* skip both words of a boxed number */
      if(here->prim == prim_number)
	here++;
      continue;
    }
    cell there = here->ptr;
    if(there[0].ptr == NULL) {
      /* already copied */
      *here = there[1];
    } else {
      here->ptr = ptr;
      ptr[0] = there[0];
      ptr[1] = there[1];
      there[0].ptr = NULL;
      there[1].ptr = ptr;
      ptr += 2;
    }
  }

  free(heap_lo);

  heap_lo = new;
  heap_hi = new + heap_size;
  heap_ptr = ptr;
  if(ptr > new + heap_size / 2)
    heap_size *= 2;

  return(new);
}

static inline cell need(cell c, int n) {
  if(heap_ptr + 2*n < heap_hi)
    return(c);
  else
    return(cheney(c));
}

static inline word cons(word w0, word w1) {
  assert(heap_ptr < heap_hi);
  cell c = heap_ptr;
  c[0] = w0;
  c[1] = w1;
  heap_ptr += 2;
  word w = { c };
  return(w);
}

static inline cell unwind(cell c, word *arg) {
  cell prev = c[1].ptr;
  if(prev == NULL)
    return(NULL);
  *arg = c[1] = prev[0];
  prev[0].ptr = c;
  return(prev);
}

static inline cell numarg(cell c, double *arg) {
  word box;
  c = unwind(c, &box);
  if(c == NULL)
    return(NULL);
  assert(!primitive(box) && box.ptr[0].prim == prim_number);
  *arg = box.ptr[1].num;
  return(c);
}

static inline void numval(cell c, double val) {
  if(c == NULL)
    return;
  cell n = c[0].ptr;
  n[0].prim = prim_number;
  n[1].num = val;
}

#define numprim1(name, fun)			\
  case(prim_##name): {				\
    double v;					\
    c = numarg(c, &v);				\
    numval(c, fun(v));				\
  } continue

#define numprim2(name, expr)			\
  case(prim_##name): {				\
    double u, v;				\
    c = numarg(c, &u);				\
    c = numarg(c, &v);				\
    numval(c, expr);				\
  } continue

static void eval(cell c) {
  while(c) {
    switch(c[0].prim) {
      default: {
	cell next = c[0].ptr;
	c[0] = next[1];
	next[1].ptr = c;
	c = next;
      } continue;
      case(prim_I): {
	word arg;
	c = unwind(c, &arg);
	if(c) c[0] = arg;
      } continue;
      case(prim_J): {
	word t, f;
	c = unwind(c, &t);
	c = unwind(c, &f);
	if(c) c[0] = f;
      } continue;
      case(prim_K): {
	word t, f;
	c = unwind(c, &t);
	c = unwind(c, &f);
	if(c) c[0] = t;
      } continue;
      case(prim_S): {
	word f, g, x;
	c = need(c, 3);
	c = unwind(c, &f);
	c = unwind(c, &g);
	c = unwind(c, &x);
	if(c) c[0] = cons(cons(f,x),cons(g,x));
      } continue;
      case(prim_number): {
	word n, k;
	c = unwind(c, &n); /* unboxed */
	c = unwind(c, &k);
	if(c) {
	  n = c[0].ptr[0]; /* boxed */
	  /* pass to continuation */
	  c[0].ptr[0] = k;
	  c[0].ptr[1] = n;
	}
      } continue;
      numprim1(floor, floor);
      numprim1(ceil, ceil);
      numprim1(abs, fabs);
      numprim1(neg, -);
      numprim2(add, u+v);
      numprim2(sub, u-v);
      numprim2(mul, u*v);
      numprim2(div, u/v);
      numprim2(mod, u-v*floor(u/v));
      numprim2(pow, pow(u,v));
    }
  }
}
