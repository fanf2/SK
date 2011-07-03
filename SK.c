#include <stdbool.h>
#include <stdlib.h>

typedef union word word, *cell;

typedef cell prim(cell app);

union word {
  word *ptr;
  prim *fun;
  double num; /* only used in the second word of a cell */
};

word *heap_lo, *heap_ptr, *heap_hi;

size_t heap_size = 1<<10;

/*
 * If a word doesn't point into the heap it must be a primitive function
 * (unless it is the second word of a cell containing a boxed number).
 */
static inline bool primitive(word w) {
  return(w.ptr < heap_lo || heap_hi <= w.ptr);
}

static prim numberfun;

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
      if(here->fun == numberfun)
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
  cell c = heap_ptr;
  c[0] = w0;
  c[1] = w1;
  heap_ptr += 2;
  word w = { c };
  return(w);
}

static void eval(cell c) {
  while(c != NULL) {
    if(primitive(c[0])) {
      c = c[0].fun(c);
    } else {
      cell next = c[0].ptr;
      c[0] = next[1];
      next[1].ptr = c;
      c = next;
    }
  }
}

static inline cell unwind(cell c, word *arg) {
  cell prev = c[1].ptr;
  *arg = c[1] = prev[0];
  prev[0].ptr = c;
  return(prev);
}

#define defprim(name) \
  static prim name##fun; \
  static word name = { .fun = name##fun }; \
  static cell name##fun(cell c)

defprim(I) {
  word a1;
  c = unwind(c, &a1);
  c[0] = a1;
  return(c);
}

defprim(J) {
  word t, f;
  c = unwind(c, &t);
  c = unwind(c, &f);
  c[0] = f;
  return(c);
}

defprim(K) {
  word t, f;
  c = unwind(c, &t);
  c = unwind(c, &f);
  c[0] = t;
  return(c);
}

defprim(S) {
  word f, g, x;
  c = need(c, 3);
  c = unwind(c, &f);
  c = unwind(c, &g);
  c = unwind(c, &x);
  c[0] = cons(cons(f,x),cons(g,x));
  return(c);
}

defprim(number) {
  word n, k;
  c = unwind(c, &n);
  /* actually we want the boxed number */
  n = c[0];
  c = unwind(c, &k);
  /* pass evaluated number to continuation */
  c[0].ptr[0] = k;
  c[0].ptr[1] = n;
  return(c);
}
