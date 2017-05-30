;;;; cl-libstemmer.lisp

(in-package #:cl-libstemmer)

;;; "cl-libstemmer" goes here. Hacks and glory await!

(def libstemmer-path
  (asdf:system-relative-pathname :cl-libstemmer "libstemmer_c/")
  "Where are the libstemmer sources?")

(defun wordp (x)
  "Sanity check for word-ness."
  (and (stringp x) (< (length x) 500)))

(deftype word ()
  "A string that is a word."
  '(and string (satisfies wordp)))

(cffi:define-foreign-library libstemmer
  (t (:default "libstemmer")))

(defun use-libstemmer ()
  "Try to load libstemmer; build it if possible."
  (handler-case
      (let ((cffi:*foreign-library-directories* (list libstemmer-path)))
        (cffi:use-foreign-library libstemmer))
    (error ()
      (format t "~&Building libstemmer.so...~%")
      (unless (zerop
               (uiop:run-program
                '("make" "libstemmer.so")
                :directory libstemmer-path))
        (error "Could not build libstemmer.so.")))))

(use-libstemmer)

(defparameter *default-encoding* :utf-8
  "The default encoding for use with stemmers.")

(def encodings
  '((:iso-8859-1 . "ISO_8859_1")
    (:iso-8859-2 . "ISO_8859_2")
    (:koi8-r . "KOI8_R")
    (:utf-8 . "UTF_8"))
  "Map keywords to encoding names libstemmer understands.")

(defun encoding->string (encoding)
  "Look up ENCODING in `encodings'."
  (assocdr encoding encodings))

(defstruct (%stemmer (:constructor %make-stemmer (pointer &aux (deleted nil))))
  "Wrap a C stemmer so we can track if it has been closed."
  (pointer (error "No pointer") :type cffi:foreign-pointer :read-only t)
  (deleted nil :type boolean))

(defun %close-stemmer (s)
  (unless (%stemmer-deleted s)
    (setf (%stemmer-deleted s) t)
    (sb_stemmer_delete (%stemmer-pointer s))
    s))

(defclass stemmer ()
  ((%stemmer :initarg :stemmer :type %stemmer :accessor %stemmer)
   (language :initarg :language
             :accessor language-of
             :accessor stemmer-language)
   (encoding :initarg :encoding
             :accessor encoding-of
             :accessor stemmer-encoding)
   (monitor :initform (bt:make-lock "Stemmer lock") :reader monitor))
  (:documentation "Lisp wrapper for a C stemmer."))

(defmethod closed? ((self stemmer))
  (%stemmer-deleted (%stemmer self)))

(defmethod print-object ((self stemmer) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (language encoding) self
      (format t "~a/~a" language encoding))))

(defcondition no-such-stemmer (error)
  ((language :initarg :language :accessor no-such-stemmer-language)
   (encoding :initarg :encoding :accessor no-such-stemmer-encoding))
  (:documentation "Error for an unsupport language/encoding combination.")
  (:report (lambda (c s)
             (with-slots (language encoding) c
               (format s "No such language/encoding pair: ~a/~a"
                       language encoding)))))

(defun check-open (stemmer)
  "Make sure STEMMER is not closed."
  (when (closed? stemmer)
    (error "~a is closed" stemmer)))

(defmethod initialize-instance :after ((self stemmer) &key language encoding)
  "Set up the C stemmer."
  (let* ((enc (encoding->string encoding))
         (ptr (cffi:with-foreign-strings ((lang language)
                                          (encoding enc))
                (sb_stemmer_new lang encoding))))
    (cond ((cffi:null-pointer-p ptr)
           (error 'no-such-stemmer
                  :language language
                  :encoding encoding))
          (t (let ((stemmer (%make-stemmer ptr)))
               (setf (%stemmer self) stemmer)
               (tg:finalize self
                            (lambda ()
                              (%close-stemmer stemmer))))))))

(defun close-stemmer (stemmer)
  "Close STEMMER and free the C-side stemmer."
  (synchronized (stemmer)
    (%close-stemmer (%stemmer stemmer))))

(defun load-stemmer (language &optional encoding)
  "Load a stemmer for LANGUAGE and ENCODING (which defaults to
`*default-encoding*`."
  (make 'stemmer
        :language (string-downcase language)
        :encoding (or encoding *default-encoding*)))

(defmacro with-stemmer ((var language &optional encoding)
                        &body body)
  (with-thunk (body var)
    `(call/stemmer ,language ,encoding #',body)))

(defun call/stemmer (lang enc fn)
  (let ((stemmer (load-stemmer lang enc)))
    (unwind-protect
         (funcall fn stemmer)
      (close-stemmer stemmer))))

;;; NB
;;; /** Stem a word.
;;; *
;;; *  The return value is owned by the stemmer - it must not be freed or
;;; *  modified, and it will become invalid when the stemmer is called again,
;;; *  or if the stemmer is freed.
;;; *
;;; *  The length of the return value can be obtained using sb_stemmer_length().
;;; *
;;; *  If an out-of-memory error occurs, this will return NULL.
;;; */

(defun stem-word/no-lock (stemmer word)
  (typecase word
    (word
     (when (stop-word-p word (stemmer-language stemmer))
       (return-from stem-word/no-lock word))
     (let ((encoding (encoding-of stemmer)))
       (cffi:with-foreign-string (fw word :encoding encoding)
         ;; TODO Octets by encoding.
         (let ((ptr (sb_stemmer_stem (%stemmer-pointer (%stemmer stemmer))
                                     fw
                                     (cffi::foreign-string-length fw :encoding encoding))))
           (cffi:foreign-string-to-lisp ptr :encoding encoding)))))
    (t word)))

(defun stem (stemmer word)
  (check-open stemmer)
  (synchronized (stemmer)
    (stem-word/no-lock stemmer word)))

(defun stem-all (list language &optional encoding)
  (handler-case
      (values
       (with-stemmer (s language encoding)
         (synchronized (s)
           (loop for item in list
                 collect (stem-word/no-lock s item))))
       t)
    (no-such-stemmer ()
      (values list nil))))
