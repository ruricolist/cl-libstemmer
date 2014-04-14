;;;; cl-libstemmer.asd

(defpackage #:cl-libstemmer.asdf
  (:use #:cl #:asdf))

(in-package #:cl-libstemmer.asdf)

(defun wrap-package (fn)
  (let ((*package* (find-package :cl-libstemmer)))
    (funcall fn)))

(defsystem #:cl-libstemmer
  :serial t
  :description "FFI for libstemmer"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:trivial-garbage
               #:cffi
               #:bordeaux-threads)
  :components ((:file "package")
               (:file "libstemmer-ffi"
                      :around-compile wrap-package)
               (:file "cl-libstemmer")))
