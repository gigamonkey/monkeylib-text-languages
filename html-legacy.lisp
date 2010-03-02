;;
;; Copyright (c) 2005-2007, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defmacro with-html-output ((stream &key (pretty *pretty*)) &body body)
  "DEPRECATED: used WITH-FOO-OUTPUT"
  `(with-foo-output (,stream :pretty ,pretty) ,@body))

(defmacro with-html-to-file ((file &key (pretty *pretty*)) &body body)
  (with-gensyms (stream)
    `(with-open-file (,stream ,file :direction :output :if-exists :supersede)
      (with-foo-output (,stream :pretty ,pretty)
        ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special Forms

;; This can be used to embed, say, CSS in the HTML page.
(define-html-special-operator :with-language (language processor (new-language) &body body)
  (declare (ignore language))
  (let ((new-language (if (boundp new-language)
		      (symbol-value new-language)
		      new-language)))
    (loop for exp in body do (process new-language processor exp (top-level-environment new-language)))))

