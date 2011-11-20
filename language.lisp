;;; Copyright (c) 2005-2001, Peter Seibel. All rights reserved. See COPYING for details.

(in-package :monkeylib-text-languages)

;;; Hmmmm. Might be useful to support symbol macros.

(defclass language ()
  ((special-operator-symbols
    :initform ()
    :accessor special-operator-symbols
    :documentation "symbols added to a symbol's plist to indicate it
    has been defined as a macro in LANGUAGE. These will typically be
    pushed onto this list in an :after method on initialize-instance
    so they are ordered the same as the class precedence list.")
   (macro-symbols
    :initform ()
    :accessor macro-symbols
    :documentation "symbols added to a symbol's plist to indicate it
    has been defined as a macro in LANGUAGE. These will typically be
    pushed onto this list in an :after method on initialize-instance
    so they are ordered the same as the class precedence list.")
   (input-readtable
    :initarg :input-readtable
    :accessor input-readtable
    :documentation "readtable we should use to read the input files in
    this language.")
   (input-package
    :initarg :input-package
    :accessor input-package
    :documentation "package we should use to read the input file.")
   (output-file-type
    :initarg :output-file-type
    :accessor output-file-type
    :documentation "file suffix for generated files.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primary interface

;; special-operator-symbols, macro-symbols, and ...

(defgeneric identifier (language form)
  (:documentation "Extract a symbol that identifies the form."))

(defgeneric sexp-form-p (language form)
  (:documentation "Is the given form a meaningful non-special,
  non-macro form in language."))

(defgeneric embeddable-value-form (language form environment)
  (:documentation "Return a form that will evaluate to a string
  that can be embedded in the generated output."))

(defgeneric process-sexp (language processor form environment)
  (:documentation "The basic evaluation rule for the language,
  after special operators and macro forms have been handled."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Secondary interface -- these are typically implemented in terms of
;;; the primary interface but it may be possible that for some
;;; language it would be desirable to provide more specific
;;; implemenations.

(defgeneric special-form-p (language form)
  (:documentation "Is the given form a special form in language. The
  default method probably does what you want--it extracts the form's
  identifier and looks for a special-operator-symbol on its plist."))

(defgeneric macro-form-p (language form)
  (:documentation "Is the given form a macro form in language. The
  default method probably does what you want--it extracts the form's
  identifier and looks for a macro-symbol on its plist."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler interface

;; input-readtable, input-package, output-file-type and ...

(defgeneric comment (language text)
  (:documentation "Return text as a comment."))

(defgeneric top-level-environment (language)
  (:documentation "Environment for evaluating top-level forms."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Null implementation of processor interface -- this is used for
;;; walking forms without generating any output.

(defmethod raw-string ((pp (eql nil)) string &optional newlines-p)
  (declare (ignore string newlines-p)))

(defmethod newline ((pp (eql nil))))

(defmethod freshline ((pp (eql nil))))

(defmethod indent ((pp (eql nil))))

(defmethod unindent ((pp (eql nil))))

(defmethod toggle-indenting ((pp (eql nil))))

(defmethod embed-value ((pp (eql nil)) value)
  (declare (ignore value)))

(defmethod embed-code ((pp (eql nil)) code)
  (declare (ignore code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language engine.

(defun process (language processor form environment)
  "Process FORM as an expression in LANGUAGE. The ENVIRONMENT is
provided to special forms and to the basic evaluation rule
implemented by a method on PROCESS-SEXP."
  (cond
    ((special-form-p language form) (process-special-form language processor form environment))
    ((macro-form-p language form)   (process language processor (expand-macro-form language form environment) environment))
    ((sexp-form-p language form)    (process-sexp language processor form environment))
    ((consp form)                   (embed-code processor form))
    (t                              (embed-value processor (embeddable-value-form language form environment)))))

(defgeneric process-special-form (language processor form environment))

(defgeneric expand-macro-form (language form environment))

(defmethod process-special-form (language processor form environment)
  (let* ((identifier (identifier language form))
         (special-operator (find-in-plist identifier (special-operator-symbols language))))
    (funcall special-operator language processor form environment)))

(defmethod expand-macro-form (language form environment)
  (let ((macro-function (find-in-plist (identifier language form) (macro-symbols language))))
    (funcall macro-function form environment)))

(defun fully-expand-macro-form (language form environment)
  (loop while (macro-form-p language form) do
       (setf form (expand-macro-form language form environment)))
  (mapcar #'(lambda (x) (if (macro-form-p language x) (fully-expand-macro-form language x environment) x)) form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default methods.

(defmethod identifier ((language t) (form cons))
  "Reasonable default for languages with a Lispy syntax."
  (and (symbolp (car form)) (car form)))

(defmethod identifier ((language t) (form t))
  (error "Malformed expression for ~a: ~s" language form))

(defmethod special-form-p ((language t) (form t)) nil)

(defmethod special-form-p ((language t) (form cons))
  (let ((identifier (identifier language form)))
    (and identifier (find-in-plist identifier (special-operator-symbols language)))))

(defmethod macro-form-p ((language t) (form t)) nil)

(defmethod macro-form-p ((language t) (form cons))
  (let ((identifier (identifier language form)))
    (and identifier (find-in-plist identifier (macro-symbols language)))))

(defmethod sexp-form-p ((language t) form)
  "Suitable default for languages in which all forms that are not
  special or macros have some meaning. Languages that allow
  embedded code and embedded values will need their own
  specialization of this method."
  (declare (ignore form))
  t)

(defmethod embeddable-value-form ((language t) form environment)
  "Reasonable default. Languages that need to escape certain
characters will need their own specializations of this method."
  (declare (ignore environment))
  `(princ-to-string ,form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros -- typically specific languages will provide their own
;;; definitional macros that will expand into these two macros.

(defmacro define-special-operator (name special-operator-symbol (language processor &rest other-parameters) &body body)
  (with-gensyms (whole)
    (multiple-value-bind (parameters environment) (parse-&environment other-parameters)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name ',special-operator-symbol)
	       (lambda (,language ,processor ,whole ,environment)
		 (declare (ignorable ,environment ,language ,processor))
		 (handler-case
		     (destructuring-bind (,@parameters) (rest ,whole)
		       ,@body)
		   (error (e)
		     (error 'foo-syntax-error :form ,whole :cause e)))))))))

(define-condition foo-syntax-error ()
  ((form :initarg :form :accessor form-of)
   (cause :initarg :cause :accessor cause-of :initform nil)))

(defmethod print-object ((c foo-syntax-error) stream)
  (print-unreadable-object (c stream)
    (format stream "in form: ~s; caused by: ~a" (form-of c) (cause-of c))))

(defmacro define-macro (name macro-symbol (&rest parameters) &body body)
  (with-gensyms (whole namevar)
    (multiple-value-bind (parameters environment) (parse-&environment parameters)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name ',macro-symbol)
	       (lambda (,whole ,environment)
		 (declare (ignorable ,environment))
		 (handler-case
		     (destructuring-bind (,@(normalize-macro-lambda-list parameters namevar)) ,whole
		       (declare (ignore ,namevar))
		       ,@body)
		   (error (e)
		     (error 'foo-syntax-error :form ,whole :cause e)))))))))

(defun parse-&environment (parameters)
  "Parse out an optional &environment parameter and return the
parameter list without it and the name of the parameter."
  (let ((cons (member '&environment parameters)))
    (if cons
     (values
      (nconc (ldiff parameters cons) (cddr cons))
      (cadr cons))
     (values parameters (make-symbol (symbol-name 'no-environment))))))

(defun normalize-macro-lambda-list (parameters namevar)
  "Create a destructuring-lambda list that can parse a whole
macro form, including an optional &whole parameter and a
parameter to eat up the macro name."
  (let* ((back (if (eql (car parameters) '&whole) (cddr parameters) parameters))
	 (front (ldiff parameters back)))
    `(,@front ,namevar ,@back)))

(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))

(defun sexp->ops (language body environment)
  (loop with compiler = (make-instance 'text-compiler)
     for form in body do (process language compiler form environment)
     finally (return (ops compiler))))

(defun emit (language body environment)
  (process language (get-pretty-printer) body environment))

(defun compile-special-op-body (processor body)
  "Code generator generator."
  (loop for thing in body collect
       (etypecase thing
	 (string `(raw-string ,processor ,thing ,(not (not (find #\Newline thing)))))
	 (cons thing)
	 (keyword
	  (ecase thing
	    (:newline `(newline ,processor))
	    (:freshline `(freshline ,processor))
	    (:indent `(indent ,processor))
	    (:unindent `(unindent ,processor)))))))

(defun case-preserving-readtable ()
  (let ((readtable (copy-readtable)))
    (setf (readtable-case readtable) :preserve)
    readtable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for top-level language functions and macros.

(defun find-in-plist (identifier symbols)
  "Find the first value on an identifier's plist given a list of
symbols. This is used to implement inheritance of special forms "
  (some (lambda (sym) (get identifier sym)) symbols))

(defun emit-for-language (language-class sexp)
  (let ((lang (make-instance language-class)))
    (process lang (get-pretty-printer) sexp (top-level-environment lang))))

(defmacro define-language (name parent)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
      (defclass ,name (,parent) ()))
     (define-language-macro ,name)))

(defmacro define-language-macro (name)
  `(defmacro ,name (&whole whole &body body)
     (declare (ignore body))
     `(macrolet ((,(car whole) (&body body)
		   (let* ((lang (make-instance ',(car whole)))
			  (env (top-level-environment lang)))
		     (codegen-text (sexp->ops lang body env) ,*pretty*))))
	,@(if *pretty*
	      `((let ((*text-pretty-printer* (get-pretty-printer))) ,whole))
	      `(,whole)))))

