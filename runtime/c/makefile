# On msys64:    make install PREFIX=/mingw64
# On linux/bsd: make install
PREFIX ?= /usr/local
VERSION ?= 0.9.3

ROOT    := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

INC = inc/*.h
PC_IN = pc/andy_runtime.pc.in
PC = pc/andy_runtime.pc


CC      := clang
CFLAGS  := -std=c23 -Wall -Wextra -Wpedantic -O3 -Iinc

SRC     := $(wildcard src/*.c)
OBJ     := $(SRC:.c=.o)
LIB     := libandy_runtime.a

AR      := ar

LDFLAGS += $(ROOT)

.PHONY: all clean test install

all: $(LIB)

$(LIB): $(OBJ)
	$(AR) rcs $@ $^

src/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

test: $(LIB)
	$(CC) $(CFLAGS) tests/runtime_test.c -L$(LDFLAGS) -landy_runtime -o tests/runtime_test
	./tests/runtime_test

clean:
	rm -f src/*.o $(LIB) tests/runtime_test

$(PC): $(PC_IN)
	sed -e "s|@PREFIX@|$(PREFIX)|g" \
	    -e "s|@VERSION@|$(VERSION)|g" \
	    $< > $@

install: $(PC)
	mkdir -p $(PREFIX)/lib $(PREFIX)/include/andy
	cp $(LIB) $(PREFIX)/lib/
	cp $(INC) $(PREFIX)/include/andy
	mkdir -p $(PREFIX)/lib/pkgconfig
	cp $(PC) $(PREFIX)/lib/pkgconfig/
