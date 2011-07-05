#define IO(n)     { .ptr = initial_orders+n*2 }
#define IOprim(n) { .prim = prim_##n }
#define IOnum(n)  { .num  = n }
word initial_orders[] = {
  IO(1), IOprim(I),
  IO(2), IOprim(exit),
  IO(3), IOprim(print),
  IO(5), IO(4),
  IOprim(box_num), IOnum(2),
  IO(7), IO(6),
  IOprim(box_num), IOnum(1),
  IO(8), IOprim(add),
  IO(10), IO(9),
  IOprim(C), IOprim(B),
  IOprim(B), IO(11),
  IOprim(C), IOprim(B),
};
