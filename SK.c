#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
  primin = -1,
  special_forward, special_number,
  prim_Y, prim_I, prim_J, prim_K,
  prim_S, prim_C, prim_B,
  prim_SS, prim_CC, prim_BB,
  prim_exit, prim_print, prim_putc, prim_getc,
  prim_floor, prim_ceil, prim_abs, prim_neg,
  prim_add, prim_sub, prim_mul, prim_div, prim_mod, prim_pow,
  primax
} prim;

static const char *primname[] = {
  "nil", "number",
  "Y", "I", "J", "K",
  "S", "C", "B", "SS", "CC", "BB",
  "exit", "print", "putc", "getc",
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
  return(primin < w.prim && w.prim < primax);
}

static inline bool isnumber(cell c) {
  return(c[0].prim == special_number);
}

static void dump(word w, int in, int mode) {
  cell c = w.ptr;
  if(primitive(w)) {
    printf("%s", primname[w.prim]);
  } else if(isnumber(c)) {
    printf("%g", c[1].num);
  } else {
    if(in != mode) printf("(");
    dump(c[0], 0, mode && in);
    printf(" ");
    dump(c[1], 1, mode);
    if(in != mode) printf(")");
  }
}

static void edump(cell c) {
  dump(c[0], 0, 0);
  printf(" -- ");
  dump(c[1], 1, 1);
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
      if(isnumber(here)) here++;
      continue;
    }
    cell there = here->ptr;
    if(there[0].prim == special_forward) {
      /* already copied */
      *here = there[1];
    } else {
      here->ptr = ptr;
      ptr[0] = there[0];
      ptr[1] = there[1];
      there[0].prim = special_forward;
      there[1].ptr = ptr;
      ptr += 2;
    }
  }
  printf("cheney copied %ld words\n", (long)(ptr-new));
  free(heap_lo);
  heap_lo = new;
  heap_hi = new + heap_size;
  heap_ptr = ptr;
  while(ptr > new + heap_size / 2)
    heap_size *= 2;
  return(new);
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

#define need(n) \
  ((void)((heap_hi < heap_ptr + 2*n) && (c = cheney(c))))

#include "initial-orders.h"

//    <- rewind    unwind ->
//
//  [ | ]  [3| ]  [3| ]  [3| ]
//   v        ^      ^      ^
//  [ |3]  [ | ]  [2| ]  [2| ]
//   v      v        ^      ^
//  [ |2]  [ |2]  [ | ]  [1| ]
//   v      v      v        ^
//  [f|1]  [f|1]  [f|1]  [f| ]
//
int main(void) {
  word a1, a2, a3, a4;
  double v, w;
  cell r, c = initial_orders;
  heap_size = 2*sizeof(initial_orders)
              /sizeof(*initial_orders);
  for(;;) {
    edump(c);
    switch(c[0].prim) {
      default: { /* unwind */
	cell next = c[0].ptr;
	c[0] = next[1];
	next[1].ptr = c;
	c = next;
      } continue;
#define rewind(a) do {		\
	cell prev = c[1].ptr;	\
	a = c[1] = prev[0];	\
	r = prev[0].ptr = c;	\
	c = prev;		\
      } while(0)
#define rewind1          rewind(a1)
#define rewind2 rewind1; rewind(a2)
#define rewind3 rewind2; rewind(a3)
#define rewind4 rewind3; rewind(a4)
#define result(r0,r1) (r[0] = (r0), r[1] = (r1))
      case(prim_I): { /* identity */
	rewind1;
	c[0] = a1;
      } continue;
      case(prim_J): { /* J t f -> f */
	rewind2;
	result((word){ .prim = prim_I }, a2);
	c[0] = a2; /* shortcut */
      } continue;
      case(prim_K): { /* K t f -> t */
	rewind2;
	result((word){ .prim = prim_I }, a1);
	c[0] = a1; /* shortcut */
      } continue;
      case(prim_S): { /* S f g x -> (f x) (g x) */
	need(2); rewind3;
	result(cons(a1,a3),cons(a2,a3));
      } continue;
      case(prim_C): { /* C f g x -> (f x) (g) */
	need(1); rewind3;
	result(cons(a1,a3),a2);
      } continue;
      case(prim_B): { /* C f g x -> (f) (g x) */
	need(1); rewind3;
	result(a1,cons(a2,a3));
      } continue;
      case(prim_SS): { /* SS e f g x -> (e (f x)) (g x) */
	need(3); rewind4;
	result(cons(a1,cons(a2,a4)),cons(a3,a4));
      } continue;
      case(prim_CC): { /* CC e f g x -> (e (f x)) (g) */
	need(2); rewind4;
	result(cons(a1,cons(a2,a4)),a3);
      } continue;
      case(prim_BB): { /* BB e f g x -> (e) (f (g x)) */
	need(2); rewind4;
	result(a1,cons(a2,cons(a3,a4)));
      } continue;
      case(prim_Y): { /* recursion Y f -> f (Y f) */
	rewind1;
	result(a1, (word){ r });
      } continue;
      case(special_number): { /* num N k -> k (num N) */
	rewind2;
	a1 = r[0]; /* boxed number */
	result(a2,a1);
      } continue;
      case(prim_exit): {
	exit(0);
      } continue;
#define numarg(a,n) do {				\
	rewind(a);					\
	assert(!primitive(a) && isnumber(a.ptr));	\
	n = a.ptr[1].num;				\
    } while(0)
#define numarg1          numarg(a1,v)
#define numarg2 numarg1; numarg(a2,w)
#define numprim(N, name, val)			\
      case(prim_##name): {			\
	numarg##N;				\
	r[0].prim = special_number;		\
	r[1].num = val;				\
      } continue
      numprim(1, floor, floor(v));
      numprim(1, ceil, ceil(v));
      numprim(1, abs, fabs(v));
      numprim(1, neg, -v);
      numprim(2, add, v+w);
      numprim(2, sub, v-w);
      numprim(2, mul, v*w);
      numprim(2, div, v/w);
      numprim(2, mod, v-w*floor(v/w));
      numprim(2, pow, pow(v,w));
      case(prim_print): {/* print n k w -> k w */
	numarg1;
	rewind2;
	printf("%g\n", v);
	r[0] = a1;
      } continue;
    }
  }
}
