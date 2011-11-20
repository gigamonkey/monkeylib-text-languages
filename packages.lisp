;;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;;
;;; See COPYING for details.

(in-package :cl-user)

(defpackage :monkeylib-text-languages
  (:use :cl
	:monkeylib-text-output
	:com.gigamonkeys.macro-utilities)
  (:export
   :case-preserving-readtable
   :comment
   :compile-special-op-body
   :define-macro
   :define-special-operator
   :define-language
   :define-language-macro
   :embeddable-value-form
   :emit
   :emit-for-language
   :environment
   :expand-macro-form
   :fully-expand-macro-form
   :foo-syntax-error
   :identifier
   :input-package
   :input-readtable
   :language
   :macro-form-p
   :macro-symbols
   :output-file-type
   :parse-&environment
   :process
   :process-sexp
   :process-special-form
   :self-evaluating-p
   :sexp->ops
   :sexp-form-p
   :special-form-p
   :special-operator-symbols
   :top-level-environment

   ;; Don't like these names. Should probably be a single GF named
   ;; generate specializing on the input and output
   :generate-from-file
   :generate-from-string
   :generate-from/to-streams
   :generate-from-sexp
))

