typedef union word word;

typedef void prim(word *);

union word {
  word *ptr;
  prim *fun;
  double num; /* only for the second word in a cell */
};

word *heap_lo, *heap_ptr, *heap_hi;

size_t heap_size = 1<<10;

static inline word *cons(word w0, word w1) {
  assert(heap_ptr < heap_hi);
  word *cell = heap_ptr;
  heap_ptr += 2;
  cell[0] = w0;
  cell[1] = w1;
  return(cell);
}

/*
 * If the first word in a cell doesn't point to the heap
 * it must be a primitive function.
 */
static inline bool primitive(word w) {
  return(w.ptr < heap_lo || heap_hi <= w.ptr);
}

static word *cheney(word *root) {
  word *new = malloc(heap_size * sizeof(*new));

  word *ptr = new;
  ptr[0] = root[0];
  ptr[1] = root[1];
  ptr += 2;

  for(word *here = new; here < ptr; here++) {
    if(primitive(*here)) {
      /* do not try to treat numbers as pointers */
      if(here->prim = number)
	here++;
      continue;
    }
    word *there = here->ptr;
    if(there[0].ptr == NULL)
      *here = there[1];
    else {
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
