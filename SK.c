#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
  primin,
  prim_I, prim_J, prim_K, prim_S, prim_Y,
  prim_number, prim_print, prim_putc, prim_getc,
  prim_floor, prim_ceil, prim_abs, prim_neg,
  prim_add, prim_sub, prim_mul, prim_div, prim_mod, prim_pow,
  primax
} prim;

static const char *primname[] = {
  "nil",
  "I", "J", "K", "S", "Y",
  "number", "print", "putc", "getc",
  "floor", "ceil", "abs", "neg",
  "+", "-", "*", "/", "%", "^"
};

typedef union word {
  union word *ptr;
  prim prim;
  double num; /* only used in the second word of a cell */
} word, *cell;

word *heap_lo, *heap_ptr, *heap_hi;
size_t heap_size;

static inline bool primitive(word w) {
  return(primin <= w.prim && w.prim < primax);
}

static void dump(cell c) {
  if(primitive(c[0])) {
    if(c[0].prim == prim_number) {
      printf("%g", c[1].num);
      return;
    }
    printf("%s", primname[c[0].prim]);
  } else {
    dump(c[0].ptr);
  }
  if(primitive(c[1])) {
    printf(" %s", primname[c[1].prim]);
  } else {
    printf(" (");
    dump(c[1].ptr);
    printf(")");
  }
}

static void rdump(word w) {
  if(primitive(w)) {
    printf(" %s", primname[w.prim]);
  } else {
    if(primitive(w.ptr[0])) {
      printf(" %s", primname[w.ptr[0].prim]);
    } else {
      printf(" (");
      dump(w.ptr[0].ptr);
      printf(")");
    }
    rdump(w.ptr[1]);
  }
}

static void edump(cell c) {
  if(primitive(c[0])) {
    if(c[0].prim == prim_number) {
      printf("%g", c[1].num);
      return;
    }
    printf("%s", primname[c[0].prim]);
  } else {
    dump(c[0].ptr);
  }
  printf(" --");
  rdump(c[1]);
  printf("\n");
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
  cell n = c[0].ptr;
  n[0].prim = prim_number;
  n[1].num = val;
}

static void eval(cell c) {
  while(c) {
    edump(c);
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
	cell r;
	c = need(c, 2);
	c = unwind(c, &f);
	c = unwind(c, &g);
	c = unwind(r=c, &x);
	if(c) {
	  r[0] = cons(f,x);
	  r[1] = cons(g,x);
	}
      } continue;
      case(prim_Y): {
	word fun;
	cell r;
	c = unwind(r=c, &fun);
	if(c) {
	  r[0] = fun;
	  r[1].ptr = r;
	}
      } continue;
      case(prim_number): {
	word n, k;
	cell r;
	c = unwind(c, &n); /* unboxed */
	c = unwind(r=c, &k);
	if(c) {
	  n = r[0]; /* boxed */
	  r[0] = k;
	  r[1] = n;
	}
      } continue;
      case(prim_print): {
	double v;
	word k, w;
	cell r;
	c = numarg(c, &v);
	c = unwind(c, &k);
	c = unwind(r=c, &w);
	if(c) {
	  printf("%g\n", v);
	  /* print n k w -> k w */
	  r[0] = k;
	}
      } continue;

#define numprim1(name, fun)		\
      case(prim_##name): {		\
	double v;			\
	c = numarg(c, &v);		\
	if(c) numval(c, fun(v));	\
      } continue

      numprim1(floor, floor);
      numprim1(ceil, ceil);
      numprim1(abs, fabs);
      numprim1(neg, -);

#define numprim2(name, expr)		\
      case(prim_##name): {		\
	double u, v;			\
	c = numarg(c, &u);		\
	c = numarg(c, &v);		\
	if(c) numval(c, expr);		\
      } continue

      numprim2(add, u+v);
      numprim2(sub, u-v);
      numprim2(mul, u*v);
      numprim2(div, u/v);
      numprim2(mod, u-v*floor(u/v));
      numprim2(pow, pow(u,v));

    }
  }
}

word IO[] = {
  [0].ptr = IO+2, [1].ptr = NULL,
  [2].ptr = IO+4, [3].prim = prim_I, // w
  [4].ptr = IO+6, [5].prim = prim_I, // k
  [6].prim = prim_print, [7].ptr = IO+8,
  [8].prim = prim_number, [9].num = 42
};

int main(void) {
  heap_size = sizeof(IO)/sizeof(*IO);
  eval(IO);
  return(0);
}
