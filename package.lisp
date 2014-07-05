;;;; package.lisp

(defpackage #:cl-libstemmer
  (:use #:cl #:alexandria #:serapeum)
  (:export :*default-encoding*
           :stemmer :stemmer-language :stemmer-encoding
           :stem-all
           :with-stemmer :stem
           :load-stemmer :close-stemmer
           :no-such-stemmer
           :no-such-stemmer-language
           :no-such-stemmer-encoding
           :stop-word-p :list-stop-words)
  (:nicknames #:libstemmer))
