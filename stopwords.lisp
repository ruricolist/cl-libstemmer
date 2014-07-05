(in-package #:libstemmer)

;;; NB All of the included stopwords files have been re-encoded as
;;; UTF-8.

;; TODO Use tries.

(defun snarf-stopwords-file (file)
  (let* ((string (read-file-into-string file))
         (lines (lines string))
         (words (remove-if #'emptyp
                           (mapcar #'trim-whitespace
                                   (mapcar (lambda (line)
                                             (subseq line 0 (position #\| line)))
                                           lines)))))
    words))

(defun snarf-stopwords (lang)
  (let ((file (build-path (asdf:system-relative-pathname
                           :cl-libstemmer
                           "stopwords/")
                          (make-pathname :name lang :type "txt"))))
    (snarf-stopwords-file file)))

(defun stopwords ()
  (let* ((dir (asdf:system-relative-pathname :cl-libstemmer "stopwords/"))
         (langs (mapcar #'pathname-name
                        (directory (merge-pathnames "*.txt" dir)))))
    (loop for lang in langs
          collect (cons lang
                        (set-hash-table (snarf-stopwords lang)
                                        :strict nil
                                        :test 'equal)))))

(defparameter *stopwords*
  (load-time-value
   (alist-hash-table
    (stopwords)
    :test 'equal)
   t))

(defun stop-words (lang &optional (table *stopwords*))
  (gethash (string-downcase lang) table #.(dict)))

(defun stop-word-p (word lang &key (table *stopwords*))
  (check-type word string)
  (values (gethash word (stop-words lang table))))

(defun list-stop-words (lang)
  (if-let (table (stop-words lang))
    (hash-table-values table)
    '()))
