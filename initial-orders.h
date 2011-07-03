#define IO initial_orders
word initial_orders[] = {
  [0].ptr = IO+2, [1].ptr = NULL,
  [2].ptr = IO+4, [3].prim = prim_I, // w
  [4].ptr = IO+6, [5].prim = prim_exit, // k
  [6].ptr = IO+10, [7].ptr = IO+8,
  [8].prim = special_number, [9].num = 42,
  [10].ptr = IO+12, [11].prim = prim_print,
  [12].ptr = IO+14, [13].prim = prim_K,
  [14].prim = prim_S, [15].prim = prim_K,
};
