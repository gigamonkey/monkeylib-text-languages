(in-package :cl-user)

(defpackage :com.gigamonkeys.foo.templates
  (:use :cl
	:com.gigamonkeys.foo
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.markup
	:com.gigamonkeys.utilities)
  (:export
   :template
   :clear-template-cache
   :*template-file*
   :add-template-component))