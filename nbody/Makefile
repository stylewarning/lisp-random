.PHONY: all bench clean

CC      = gcc
CFLAGS  = -O3 -ffast-math -march=native
SBCL    = sbcl --noinform --no-userinit --no-sysinit

N ?= 50000000

all: nbody-c nbody-lisp nbody-lisp-conventional

nbody-c: third_party/nbody.c
	$(CC) $(CFLAGS) -o $@ $< -lm

nbody-lisp-conventional: conventional.lisp
	$(SBCL) \
		--load "conventional.lisp" \
		--eval '(sb-ext:save-lisp-and-die "nbody-lisp-conventional" :toplevel (function main) :purify t :executable t)'

nbody-lisp: library.lisp impl.lisp
	$(SBCL) \
		--load "library.lisp" \
		--load "impl.lisp" \
		--eval '(sb-ext:save-lisp-and-die "nbody-lisp" :toplevel (function nsim:main) :purify t :executable t)'

define median3
	@out1=$$($(1) 2>&1); out2=$$($(1) 2>&1); out3=$$($(1) 2>&1); \
	energy=$$(echo "$$out1" | head -1); \
	t1=$$(echo "$$out1" | sed -n 's/timing: \([0-9]*\) ms/\1/p'); \
	t2=$$(echo "$$out2" | sed -n 's/timing: \([0-9]*\) ms/\1/p'); \
	t3=$$(echo "$$out3" | sed -n 's/timing: \([0-9]*\) ms/\1/p'); \
	median=$$(printf '%s\n' $$t1 $$t2 $$t3 | sort -n | sed -n '2p'); \
	echo "$$energy"; \
	echo "runs: $$t1 $$t2 $$t3 ms"; \
	echo "median: $$median ms"
endef

bench: all
	@echo "=== C (gcc -O3 -ffast-math) ==="
	$(call median3,./nbody-c $(N))
	@echo ""
	@echo "=== Lisp (SBCL, conventional loops) ==="
	$(call median3,./nbody-lisp-conventional $(N))
	@echo ""
	@echo "=== Lisp (SBCL, kernel syntax) ==="
	$(call median3,./nbody-lisp $(N))

clean:
	rm -f nbody-c nbody-lisp nbody-lisp-conventional *.fasl
