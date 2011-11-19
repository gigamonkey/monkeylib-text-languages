;;
;; Copyright (c) 2005-2007, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.xml)

(defclass xml (language)
  ((name-converter :initarg :name-converter :initform #'string-downcase :accessor name-converter))
  (:default-initargs
   :special-operator-symbol 'xml-special-operator
    :macro-symbol 'xml-macro
    :input-readtable (case-preserving-readtable)
    :input-package (find-package :keyword)
    :output-file-type "xml"))

(defmacro define-xml-language (name &body element-defs)
  (let ((block-elements (cdr (assoc :block-elements element-defs)))
        (paragraph-elements (cdr (assoc :paragraph-elements element-defs)))
        (preserve-whitespace-elements (cdr (assoc :preserve-whitespace element-defs))))
    `(progn
       (define-language ,name xml)
       (defmethod top-level-environment ((language ,name))
         (new-env
          'block-elements ',block-elements
          (new-env
           'paragraph-elements ',paragraph-elements
           (new-env
            'preserve-whitespace-elements ',preserve-whitespace-elements
            (call-next-method))))))))

(defparameter *element-escapes* "<>&")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defun emit-xml (sexp) (emit-for-language 'xml sexp))

(define-language-macro xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language implementation

(defmethod identifier ((language xml) form)
  (let ((first (car form)))
    (cond
      ((symbolp first) first)
      ((and (consp first) (symbolp (car first))) (car first))
      (t (error "Malformed xml-sexp form: ~s" form)))))

(defmethod sexp-form-p ((language xml) form)
  (or (self-evaluating-p form) (cons-form-p form)))

(defmethod embeddable-value-form ((language xml) form environment)
  `(escape (princ-to-string ,form) ,(escapes environment)))

(defmethod process-sexp ((language xml) processor form environment)
  (if (self-evaluating-p form)
      ;; Since the form is self-evaluating, we can escape it at
      ;; compile time which allows it to be included in the static
      ;; output optimization.
      (raw-string processor (escape (princ-to-string form) (escapes environment)) t)
      (process-cons-sexp-xml language processor form environment)))

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
           (and (consp (car form)) (funcall test (caar form))))))

(defun parse-cons-form (sexp)
  (if (consp (first sexp))
    (parse-explicit-attributes-sexp sexp)
    (parse-implicit-attributes-sexp sexp)))

(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop with tag = (first sexp)
     for rest on (rest sexp) by #'cddr
     while (and (keywordp (first rest)) (rest rest))
     when (second rest)
     collect (first rest) into attributes and
     collect (second rest) into attributes
     end
     finally (return (values tag attributes rest))))

(defun process-cons-sexp-xml (language processor form environment)
  (when (in-attribute-p environment)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind (tag attributes body) (parse-cons-form form)
    (emit-open-tag     language processor tag body attributes environment)
    (emit-element-body language processor tag body environment)
    (emit-close-tag    language processor tag body environment)))


(defgeneric emit-open-tag (language processor tag body-p attributes environment))
(defgeneric emit-attributes (language processor attributes environment))
(defgeneric emit-element-body (language processor tag body environment))
(defgeneric emit-close-tag (language processor tag body-p environment))

(defmethod emit-open-tag ((language xml) processor tag body-p attributes environment)
  (when (or (paragraph-element-p tag environment) (block-element-p tag environment))
    (freshline processor))
  (raw-string processor (format nil "<~a" (funcall (name-converter language) tag)))
  (emit-attributes language processor attributes environment)
  (raw-string processor
              (if (or body-p (non-empty-element-p tag environment)) ">" "/>")))

(defmethod emit-attributes ((language xml) processor attributes environment)
  (loop for (k v) on attributes by #'cddr do
       (raw-string processor (format nil " ~a='" (funcall (name-converter language) k)))
       (process language processor (if (eql v t) k v) (in-attribute environment))
       (raw-string processor "'")))

(defmethod emit-element-body ((language xml) processor tag body environment)
  (when (block-element-p tag environment)
    (unless (preserve-whitespace-p tag environment) (freshline processor))
    (indent processor))
  (when (preserve-whitespace-p tag environment) (toggle-indenting processor))
  (dolist (item body)  (process language processor item environment))
  (when (preserve-whitespace-p tag environment) (toggle-indenting processor))
  (when (block-element-p tag environment)
    (unindent processor)
    (unless (preserve-whitespace-p tag environment) (freshline processor))))

(defmethod emit-close-tag ((language xml) processor tag body-p environment)
  (when (or body-p (non-empty-element-p tag environment))
    (raw-string processor (format nil "</~a>" (funcall (name-converter language) tag))))
  (when (or (paragraph-element-p tag environment) (block-element-p tag environment))
    (freshline processor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler implementation

(defmethod comment ((language xml) text)
  (format nil "~&<!--~&~a~&-->" text))

(defmethod top-level-environment ((language xml))
  (new-env 'escapes *element-escapes* nil))

(defun new-env (key value env)
  (acons key value env))

(defun environment-value (key env)
  (cdr (assoc key env)))

(defun escapes (env)
  (if (not env) (break "null env."))
  (environment-value 'escapes env))

(defun in-attribute (env)
  (new-env 'in-attribute t (new-env 'escapes "<>&\"'" env)))

(defun in-attribute-p (env)
  (environment-value 'in-attribute env))

(defun block-element-p (tag env)
  (let ((be (environment-value 'block-elements env)))
    (or
     (find tag be)
     (and (= 1 (length be)) (eql (first be) t)))))

(defun paragraph-element-p (tag env)
  (find tag (environment-value 'paragraph-elements env)))

(defun preserve-whitespace-p (tag env)
  (find tag (environment-value 'preserve-whitespace-elements env)))

(defun non-empty-element-p (tag env)
  (find tag (environment-value 'non-empty-elements env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-xml-macro (name (&rest parameters) &body body)
  (multiple-value-bind (attributes parameters)
      (parse-xml-macro-lambda-list parameters)
    (if attributes
      (generate-macro-with-attributes name attributes parameters body)
      `(define-macro ,name xml-macro (,@parameters) ,@body))))

(defun generate-macro-with-attributes (name attributes parameters body)
  (with-gensyms (form all tag tag-body)
    `(define-macro ,name xml-macro (&whole ,form &body ,all)
       (declare (ignore ,all))
       (multiple-value-bind (,tag ,attributes ,tag-body) (parse-cons-form ,form)
         (declare (ignore ,tag))
         (destructuring-bind (,@parameters) ,tag-body
           ,@body)))))

(defun parse-xml-macro-lambda-list (args)
  (let ((attr-cons (member '&attributes args)))
    (values
     (cadr attr-cons)
     (nconc (ldiff args attr-cons) (cddr attr-cons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special Forms

(defmacro define-xml-special-operator (name (language processor &rest other-parameters) &body body)
  `(define-special-operator ,name xml-special-operator (,language ,processor ,@other-parameters) ,@body))

(define-xml-special-operator :print (language processor form &environment env)
  (cond
    ((self-evaluating-p form)
     (warn "Redundant :print of self-evaluating form ~s" form)
     (process-sexp language processor form env))
    (t
     (embed-value processor (embeddable-value-form language form env)))))

(define-xml-special-operator :format (language processor &rest args &environment env)
  (if (every #'self-evaluating-p args)
    (process-sexp language processor (apply #'format nil args) env)
    (embed-value processor (embeddable-value-form language `(format nil ,@args) env))))

(define-xml-special-operator :progn (language processor &rest body &environment env)
  (loop for exp in body do (process language processor exp env)))

(define-xml-special-operator :|progn| (language processor &rest body &environment env)
  (loop for exp in body do (process language processor exp env)))

(define-xml-special-operator :noescape (language processor &rest body &environment env)
  (loop for exp in body do
       (process language processor exp (new-env 'escapes "" env))))

(define-xml-special-operator :escape (language processor &rest body &environment env)
  (loop for exp in body do
       (process language processor exp (new-env 'escapes *element-escapes* env))))

(define-xml-special-operator :attribute (language processor &rest body &environment env)
  (loop for exp in body do
       (process language processor exp (in-attribute env))))

(define-xml-special-operator :newline (language processor)
  (declare (ignore language))
  (newline processor))
