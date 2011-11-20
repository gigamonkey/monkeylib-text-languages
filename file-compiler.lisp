;;; Copyright (c) 2005-2001, Peter Seibel. All rights reserved. See COPYING for details.

(in-package :text-languages)

(defun generate (language input &optional output)
  "Input can be a sexp, a string, or a stream. Output can be an output
stream, a string, a pathname, NIL, or T. The latter two are
interpreted as they are by CL:FORMAT, NIL generates a string, T send
to *STANDARD-OUTPUT*."
  (let ((sexp (input-to-sexp language input))
        (out (output-to-stream output)))
    (let ((env (top-level-environment language)))
      (with-text-output (out :pretty t)
        (process language (get-pretty-printer) sexp env))
      (prog1
          (finish out)
        (when (or (pathnamep output) (stringp output))
          (close out))))))

(defgeneric input-to-sexp (language input)
  (:documentation "Convert input to a sexp.")

  (:method (language (input cons)) input)

  (:method (language (input string))
    (with-input-from-string (in input)
      (input-to-sexp language in)))

  (:method (language (input pathname))
    (with-open-file (in input)
      (input-to-sexp language in)))

  (:method (language (input stream))
    (let ((*readtable* (input-readtable language))
          (*package* (input-package language)))
      (loop for form = (read input nil input)
         while (not (eql form input)) collect form))))

(defgeneric output-to-stream (output)
  (:documentation "Convert output to a stream.")

  (:method ((output stream)) output)

  (:method ((output string)) (output-to-stream (pathname output)))

  (:method ((output pathname))
    (open output :direction :output :if-exists :supersede))

  (:method ((output (eql nil))) (make-string-output-stream))

  (:method ((output (eql t))) *standard-output*))

(defgeneric finish (stream)
  (:documentation "Close the output stream or return the result as necessary.")
  (:method ((stream string-stream)) (get-output-stream-string stream))
  (:method ((stream file-stream))
    (finish-output stream)
    (truename stream))
  (:method ((stream stream))
    (finish-output stream)
    stream))
