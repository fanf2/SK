typedef union word word, cell[2];

typedef void prim(cell app);

union word {
  word *ptr;
  prim *fun;
  double num; /* only for the second word in a cell */
};

word *heap_lo, *heap_ptr, *heap_hi;

size_t heap_size = 1<<10;

static inline cell cons(word w0, word w1) {
  assert(heap_ptr < heap_hi);
  cell cell = heap_ptr;
  heap_ptr += 2;
  cell[0] = w0;
  cell[1] = w1;
  return(cell);
}

/*
 * If a word doesn't point to the heap it must be a primitive function
 * (unless it is the second word of a cell containing a boxed number).
 */
static inline bool primitive(word w) {
  return(w.ptr < heap_lo || heap_hi <= w.ptr);
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
      if(here->prim = number)
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
      there[0] = NULL;
      there[1] = ptr;
      ptr += 2;
    }
  }

  heap_lo = new;
  heap_hi = new + heap_size;
  heap_ptr = ptr;
  if(ptr - new > heap_size / 2)
    heap_size *= 2;

  return(new);
}

static void eval(cell app) {
  for(;;) {
    if(primitive(app[0])) {
      app = app[0].fun(app);
    } else {
      cell next = app[0].ptr;
      app[0] = next[1];
      next[1].ptr = app;
      app = next;
    }
  }
}
