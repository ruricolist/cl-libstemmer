The [Snowball][snowball] project has defined stemming algorithms for
17 languages. Libstemmer provides these algorithms as a C library.

CL-LIBSTEMMER includes the full source of libstemmer, and will attempt
to build and load `libstemmer.so` when it is first loaded. Obviously
this will only work on a system with `make`.

The preferred way to use CL-LIBSTEMMER is with `stem-all`:

    (libstemmer:stem-all '("visible" "irradiate" "vainglorious" "habitat")
                                  :en)
    => '("visibl" "irradi" "vainglori" "habitat"), T

`stem-all` takes a list of words, a language (as a two or three letter
abbreviation) and, optionally, an encoding. If such a stemmer exists,
stem-all returns the stemmed words; otherwise, it returns the list of
words unchanged. The second value is T if stemming was actually done.

You can also stem incrementally, using `with-stemmer` and `stem`:

    (libstemmer:with-stemmer (stemmer :en)
      (libstemmer:stem stemmer "resplendent"))
    => "resplend"

There are also unbalanced `load-stemmer` and `close-stemmer`
functions. Bear in mind that loading a stemmer is relatively
expensive: for best results, stem in large batches.

Besides libstemmer itself, CL-LIBSTEMMER also includes the lists of
stop words compiled by the Snowball project.

     (libstemmer:stop-word-p "is" :es) => T

[snowball]: http://snowball.tartarus.org/index.php
