#define IO(n)     { .ptr = initial_orders+n*2 }
#define IOprim(n) { .prim = prim_##n }
#define IOnum(n)  { .num  = n }
word initial_orders[] = {
  IO(1), IOprim(I),
  IO(2), IOprim(exit),
  IO(3), IOprim(print),
  IO(5), IO(4),
  IO(14), IO(12),
  IO(7), IO(6),
  IO(14), IO(13),
  IO(8), IOprim(add),
  IO(10), IO(9),
  IOprim(C), IOprim(B),
  IOprim(B), IO(11),
  IOprim(C), IOprim(B),
  IOprim(box_num), IOnum(3),
  IOprim(box_num), IOnum(2),
  IOprim(add), IO(15),
  IOprim(box_num), IOnum(1),
};
