#define IO(n)     { .ptr = initial_orders+n*2 }
#define IOprim(n) { .prim = prim_##n }
#define IOnum(n)  { .num  = n }
word initial_orders[] = {
  IO(1), IOprim(I),
  IO(2), IO(8),
  IO(3), IO(4),
  IOprim(S), IO(4),
  IO(5), IO(6),
  IOprim(C), IOprim(B),
  IO(7), IOprim(I),
  IOprim(S), IOprim(I),
  IO(9), IO(16),
  IO(10), IO(11),
  IOprim(BB), IOprim(getc),
  IOprim(S), IO(12),
  IO(13), IOprim(exit),
  IOprim(C), IO(14),
  IOprim(gt), IO(15),
  IOprim(box_num), IOnum(0),
  IOprim(C), IOprim(putc),
};

// `X( `fun(X `arg(proc proc arg))
//     `fun(X `arg(proc proc arg)) )
//
// Y = S (C B (S I I)) (C B (S I I));
//
// main = Y \loop (
//  getc \c
//  > 0 c;
//    exit;
//    putc c loop);
//
// main = Y (BB getc (S (C (gt 0) exit)) (C putc))
