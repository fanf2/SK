#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
  primin = -1,
  prim_nil, prim_box_moved, prim_box_num, /* special */
  prim_Y, prim_I, prim_J, prim_K,
  prim_S, prim_C, prim_B,
  prim_SS, prim_CC, prim_BB,
  prim_exit, prim_print, prim_putc, prim_getc,
  prim_floor, prim_ceil, prim_abs, prim_neg,
  prim_add, prim_sub, prim_mul, prim_div, prim_mod, prim_pow,
  primax
} prim;

static const char *primname[] = {
  "nil", "moved", "num",
  "Y", "I", "J", "K",
  "S", "C", "B", "SS", "CC", "BB",
  "exit", "print", "putc", "getc",
  "floor", "ceil", "abs", "neg",
  "+", "-", "*", "/", "%", "^"
};

typedef union word {
  union word *ptr;
  prim prim;
  double num;
} word;

#define mkprim(f) ((word){ .prim = (prim_##f) })
#define mkptr(p)  ((word){ .ptr  = (p) })
#define mknum(n)  ((word){ .num  = (n) })

static inline bool isprim(word w) { return(primin < w.prim && w.prim < primax); }
static inline bool isnum(word w)  { return(w.ptr[0].prim == prim_box_num); }

word *heap_lo, *heap_ptr, *heap_hi;
size_t heap_size;

static void rdump(word w, bool brac, bool rev) {
  if(isprim(w)) {
    printf("%s", primname[w.prim]);
  } else if(isnum(w)) {
    printf("%g", w.ptr[1].num);
  } else {
    if(brac) printf("(");
    rdump(w.ptr[rev], rev, 0);
    printf(" ");
    rdump(w.ptr[!rev], !rev, rev);
    if(brac) printf(")");
  }
}

static void edump(word fun, word arg) {
  rdump(fun, 0, 0);
  printf(" -- ");
  rdump(arg, 0, 1);
  printf("\n");
}

static word cheney(word root0, word root1) {
  word *new = malloc(heap_size * sizeof(*new));
  word *ptr = new;
  ptr[0] = root0;
  ptr[1] = root1;
  ptr += 2;
  /* scan new heap word-by-word */
  for(word *here = new; here < ptr; here++) {
    if(isprim(*here)) {
      /* skip both words of a boxed number */
      if(here->prim == prim_box_num) here++;
      continue;
    }
    word *there = here->ptr;
    if(there[0].prim == prim_box_moved) {
      *here = there[1];
    } else {
      /* move from there to here */
      ptr[0] = there[0]; there[0] = mkprim(box_moved);
      ptr[1] = there[1]; there[1] = mkptr(ptr);
      here->ptr = ptr; ptr += 2;
    }
  }
  printf("cheney copied %ld pairs\n", (long)(ptr-new)/2);
  free(heap_lo);
  heap_lo = new;
  heap_hi = new + heap_size;
  heap_ptr = ptr;
  while(ptr > new + heap_size / 2)
    heap_size *= 2;
  return(mkptr(new));
}

static inline word cons(word w0, word w1) {
  assert(heap_ptr < heap_hi);
  word *box = heap_ptr;
  box[0] = w0;
  box[1] = w1;
  heap_ptr += 2;
  return(mkptr(box));
}

#define need(n)	((heap_ptr + 2*(n) < heap_hi) ? (void)(0) : \
  (void)(tmp = cheney(fun,arg), fun = tmp.ptr[0], arg = tmp.ptr[1]))

#include "initial-orders.h"

int main(void) {
  heap_size = 2*sizeof(initial_orders)
              /sizeof(*initial_orders);
  word fun = mkptr(initial_orders), arg = mkprim(nil), tmp;
#define box fun.ptr
#define result(r0,r1) (box[0] = (r0), box[1] = (r1))
  word a1, a2, a3, a4;
  double v, w;
  for(;;) {
    edump(fun,arg);
    switch(fun.prim) {
      default: { /* unwind */
	tmp = box[0]; box[0] = arg; arg = fun; fun = tmp;
      } continue;
#define rewind(a) do {						\
	tmp = fun; fun = arg; arg = box[0]; box[0] = tmp;	\
	a = box[1];						\
      } while(0)
#define rewind1          rewind(a1)
#define rewind2 rewind1; rewind(a2)
#define rewind3 rewind2; rewind(a3)
#define rewind4 rewind3; rewind(a4)
      case(prim_Y): { /* recursion Y f -> f (Y f) */
	rewind1;
	result(a1,fun);
      } continue;
      case(prim_I): { /* identity */
	rewind1;
	fun = a1;
      } continue;
      case(prim_J): { /* J t f -> f */
	rewind2;
	result(mkprim(I), a2);
	fun = a2; /* shortcut */
      } continue;
      case(prim_K): { /* K t f -> t */
	rewind2;
	result(mkprim(I), a1);
	fun = a1; /* shortcut */
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
      case(prim_box_num): { /* num N k -> k (num N) */
	rewind2;
	a1 = box[0]; /* boxed number, not bare float */
	result(a2,a1);
      } continue;
#define numarg(a,n) do {			\
	rewind(a);				\
	assert(!isprim(a) && isnum(a));	\
	n = a.ptr[1].num;			\
    } while(0)
#define numarg1          numarg(a1,v)
#define numarg2 numarg1; numarg(a2,w)
#define numprim(N, name, val)			\
      case(prim_##name): {			\
	numarg##N;				\
	result(mkprim(box_num), mknum(val));	\
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
	result(a1,a2);
      } continue;
      case(prim_exit): {
	exit(0);
      } continue;
    }
  }
}
