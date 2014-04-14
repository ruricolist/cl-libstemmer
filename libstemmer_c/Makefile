include mkinc.mak
CFLAGS=-fPIC
all: libstemmer.o stemwords
libstemmer.o: $(snowball_sources:.c=.o)
	$(AR) -cru $@ $^
libstemmer.so: $(snowball_sources:.c=.o)
	$(CC) -shared -o $@ $^
stemwords: examples/stemwords.o libstemmer.o
	$(CC) -o $@ $^
clean:
	rm -f stemwords *.so *.o src_c/*.o runtime/*.o libstemmer/*.o
