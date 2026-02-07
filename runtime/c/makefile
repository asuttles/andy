ROOT    := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

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

install:
	mkdir -p /usr/local/lib /usr/local/include/andy
	cp $(LIB) /usr/local/lib/
	cp include/*.h /usr/local/include/andy
