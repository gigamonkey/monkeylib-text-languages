;;
;; Copyright (c) 2005-2007, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.xml)

(defclass xhtml (xml)
  ()
  (:default-initargs
   :output-file-type "html"))

(defclass html (xhtml)
  ()
  (:default-initargs
   :special-operator-symbol 'html-special-operator
    :macro-symbol 'html-macro
    :input-readtable (copy-readtable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defun emit-xhtml (sexp) (emit-for-language 'xhtml sexp))

(defun emit-html (sexp) (emit-for-language 'html sexp))

(define-language-macro xhtml)

(define-language-macro html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language implementation

(defmethod emit-open-tag ((language html) processor tag body-p attributes environment)
  (declare (ignore body-p))
  (when (or (paragraph-element-p tag environment) (block-element-p tag environment))
    (freshline processor))
  (raw-string processor (format nil "<~a" tag))
  (emit-attributes language processor attributes environment)
  (raw-string processor ">"))

(defmethod emit-close-tag ((language html) processor tag body-p environment)
  (when (or body-p (not (empty-element-p tag environment)))
    (raw-string processor (format nil "</~a>" tag)))
  (when (or (paragraph-element-p tag environment) (block-element-p tag environment))
    (freshline processor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler implementation

(defmethod top-level-environment ((language xhtml))
  (new-env
   'block-elements 
   '(:body :colgroup :div :dl :fieldset :form :head :html :map :noscript
     :object :ol :optgroup :pre :script :select :style :table :tbody
     :tfoot :thead :tr :ul)
   (new-env 
    'paragraph-elements
    '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
      :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
      :td :textarea :th :title)
    (new-env 
     'preserve-whitespace-elements
     '(:pre :script :style :textarea)
     (new-env 
      'inline-elements
      '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
	:i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
	:sup :tt :var)
      (new-env
       'non-empty-elements
       '(:script :style :textarea)
       (call-next-method)))))))

(defmethod top-level-environment ((language html))
  (new-env 
   'empty-elements 
   '(:area :base :br :col :hr :img :input :link :meta :param)
   (call-next-method)))

(defun empty-element-p (tag env)
  (find tag (environment-value 'empty-elements env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-html-macro (name (&rest parameters) &body body)
  (multiple-value-bind (attributes parameters)
      (parse-xml-macro-lambda-list parameters)
    (if attributes
      (generate-html-macro-with-attributes name attributes parameters body)
      `(define-macro ,name html-macro (,@parameters) ,@body))))

(defun generate-html-macro-with-attributes (name attributes parameters body)
  (with-gensyms (form all tag tag-body)
    `(define-macro ,name html-macro (&whole ,form &body ,all)
       (declare (ignore ,all))
       (multiple-value-bind (,tag ,attributes ,tag-body) (parse-cons-form ,form)
	 (declare (ignore ,tag))
	 (destructuring-bind (,@parameters) ,tag-body
	   ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special Forms

(defmacro define-html-special-operator (name (language processor &rest other-parameters) &body body)
  `(define-special-operator ,name html-special-operator (,language ,processor ,@other-parameters) ,@body))

(define-html-special-operator :print (language processor form &environment env)
  (cond
    ((self-evaluating-p form)
     (warn "Redundant :print of self-evaluating form ~s" form)
     (process-sexp language processor form nil))
    (t
     (embed-value processor (embeddable-value-form language form env)))))

(define-html-special-operator :format (language processor &rest args &environment env)
  (if (every #'self-evaluating-p args)
    (process-sexp language processor (apply #'format nil args) env)
    (embed-value processor (embeddable-value-form language `(format nil ,@args) env))))

(define-html-special-operator :progn (language processor &rest body &environment env)
  (loop for exp in body do (process language processor exp env)))

(define-html-special-operator :noescape (language processor &rest body &environment env)
  (loop for exp in body do
       (process language processor exp (new-env 'escapes "" env))))

(define-html-special-operator :escape (language processor &rest body &environment env)
  (loop for exp in body do
       (process language processor exp (new-env 'escapes *element-escapes* env))))

(define-html-special-operator :attribute (language processor &rest body &environment env)
  (loop for exp in body do
       (process language processor exp (in-attribute env))))

(define-html-special-operator :newline (language processor)
  (declare (ignore language))
  (newline processor))

