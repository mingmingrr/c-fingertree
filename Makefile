build/libfingertree.o: src/libfingertree.c src/libfingertree.h
	gcc -g -c -o build/libfingertree.o src/libfingertree.c

build/main.o: src/main.c src/libfingertree.h
	gcc -g -c -o build/main.o src/main.c

build/libfingertree: build/libfingertree.o build/main.o
	gcc -g -o build/libfingertree build/libfingertree.o build/main.o

build/libfingertree.out: build/libfingertree
	( unbuffer timeout 30 valgrind build/libfingertree 2>&3 \
		| tee build/libfingertree.out ) 3>&1 1>&2 \
		| tee build/libfingertree.err

build/FingerTree.hs: src/FingerTree.hsc src/libfingertree.h
	hsc2hs -I src -o build/FingerTree.hs src/FingerTree.hsc

build/FingerTreeSpec.hs: src/FingerTreeSpec.hsc src/libfingertree.h
	hsc2hs -I src -o build/FingerTreeSpec.hs src/FingerTreeSpec.hsc

build/FingerTreeSpec: build/FingerTree.hs build/FingerTreeSpec.hs build/libfingertree.o
	ghc -o build/FingerTreeSpec -isrc -ibuild \
		-hidir build -odir build -dumpdir build \
		-main-is FingerTreeSpec build/FingerTreeSpec.hs build/libfingertree.o
	touch build/FingerTreeSpec

build/FingerTreeSpec.out: build/FingerTreeSpec
	( unbuffer timeout 30 build/FingerTreeSpec 2>&3 \
		| tee build/FingerTreeSpec.out ) 3>&1 1>&2 \
		| tee build/FingerTreeSpec.err

.PHONY: all
all: build/libfingertree.out build/FingerTreeSpec.out

.PHONY: clean
clean:
	rm -rf build/*

