;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.foo.text-output
  (:use :cl)
  (:export
   :*pretty*
   :*text-output*
   :*text-pretty-printer*
   :codegen-text
   :embed-code
   :embed-value
   :freshline
   :get-pretty-printer
   :indent
   :newline
   :ops
   :raw-string
   :text-compiler
   :toggle-indenting
   :unindent
   :with-foo-output))

(defpackage :com.gigamonkeys.foo.language
  (:use :cl
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.foo.text-output)
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
   :macro-symbol
   :output-file-type
   :parse-&environment
   :process
   :process-sexp
   :process-special-form
   :self-evaluating-p
   :sexp->ops
   :sexp-form-p
   :special-form-p
   :special-operator-symbol
   :top-level-environment))

(defpackage :com.gigamonkeys.foo.xml
  (:use :common-lisp
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.utilities
	:com.gigamonkeys.foo.text-output
	:com.gigamonkeys.foo.language
	:com.gigamonkeys.test
	:com.gigamonkeys.pathnames)
  (:export
   :&attributes 
   :cons-form-p 
   :define-html-macro 
   :define-xml-macro 
   :define-xml-language
   :emit-html 
   :emit-xml 
   :emit-xhtml 
   :html 
   :in-html-style 
   :parse-cons-form 
   :with-html-output 
   :with-html-to-file 
   :xhtml
   :xml))

(defpackage :com.gigamonkeys.foo.css
  (:use :common-lisp
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.utilities
	:com.gigamonkeys.foo.text-output
	:com.gigamonkeys.foo.language)
  (:export
   :emit-css
   :css))

(defpackage :com.gigamonkeys.foo
  (:use :common-lisp
	:com.gigamonkeys.foo.xml
	:com.gigamonkeys.foo.css
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.utilities
	:com.gigamonkeys.foo.text-output
	:com.gigamonkeys.foo.language)
  (:export 
   :&attributes
   :compile-javascript
   :cons-form-p
   :css
   :define-css-macro
   :define-html-macro
   :define-xml-macro
   :define-html-special-operator
   :emit-css
   :emit-html
   :emit-xhtml
   :emit-javascript
   :emit-xml
   :generate-from-sexp
   :generate-from-file
   :generate-from-string
   :html
   :xhtml
   :in-html-style
   :javascript-gensym
   :parse-cons-form
   :process
   :process-special-form
   :with-foo-output
   :with-html-output
   :with-html-to-file
   :xml))

(defpackage com.gigamonkeys.foo.javascript
  (:use :common-lisp
	:com.gigamonkeys.foo
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.utilities
	:com.gigamonkeys.foo.text-output
	:com.gigamonkeys.foo.language)
  (:export 
   :javascript
   :define-javascript-macro
   :javascript-gensym
   :new-env
   :method

   ;; Special operators
   :! 
   :!= 
   :!=== 
   :% 
   :%= 
   :& 
   :&& 
   :&= 
   :* 
   :*= 
   :+ 
   :++ 
   :+= 
   :- 
   :-- 
   :-= 
   :/ 
   :/= 
   :< 
   :<< 
   :<<= 
   :<= 
   := 
   :== 
   :=== 
   :> 
   :>= 
   :>> 
   :>>= 
   :>>> 
   :>>>= 
   :? 
   :@ 
   :\| 
   :\|= 
   :\|\| 
   :^ 
   :^= 
   :~
   :augment-environment ;; doesn't map to javascript construct
   :array 
   :block 
   :break 
   :comment
   :continue 
   :delete 
   :do-while 
   :for 
   :function 
   :function 
   :if 
   :in 
   :instanceof 
   :label 
   :new 
   :object 
   :prog 
   :progn 
   :progn 
   :ref 
   :return 
   :switch 
   :throw 
   :try 
   :typeof 
   :var 
   :void 
   :while 
   :with))


(defpackage com.gigamonkeys.foo.lispscript
  (:use :common-lisp
	:com.gigamonkeys.foo.language
	:com.gigamonkeys.foo.javascript)
  (:shadow :=)
  (:export :*lispscript*))


(defpackage com.gigamonkeys.foo.lispscript-tests
  (:use :common-lisp
	:com.gigamonkeys.foo
	:com.gigamonkeys.foo.xml
	:com.gigamonkeys.foo.javascript
	:com.gigamonkeys.foo.lispscript))

(defpackage com.gigamonkeys.foo.javascript.tokens (:use))

