CFLAGS= -pipe -O -g -std=c99 -pedantic -Wall -Wextra \
	-Wbad-function-cast -Wcast-align -Wcast-qual -Wformat=2 -Winit-self \
	-Winline -Wmissing-declarations -Wmissing-prototypes -Wnested-externs \
	-Wold-style-definition -Wpointer-arith -Wredundant-decls -Wshadow \
	-Wstrict-prototypes -Wswitch-default -Wswitch-enum -Wunreachable-code \
	-Wwrite-strings
LDFLAGS= -lm

all: SK hsk

SK: SK.c initial-orders.h

hsk:
	ghc --make hsk.hs
