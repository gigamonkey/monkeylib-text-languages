(in-package :com.gigamonkeys.foo.templates)

(defparameter *template-functions* (make-hash-table :test #'equal))
(defparameter *template-components* (make-hash-table :test #'equal))
(defvar *template-file*)

(defmacro template ((&rest bindings) &body (language file &key cache-file contents-modified))
  (let* ((bindings (normalize-bindings bindings))
	 (vars (mapcar #'first bindings))
	 (value-forms (mapcar #'second bindings)))
    (once-only (file cache-file contents-modified)
      (with-gensyms (fn out)
	`(let ((,fn (get-template-function ',vars ,language ,file ,*package*)))
	   (cond
	     (,cache-file
	      (when (< (or (file-write-date ,cache-file) 0) ,contents-modified)
		(with-open-file (,out ,cache-file :direction :output :if-exists :supersede)
		  (with-foo-output (,out) (funcall ,fn ,@value-forms))))
	      (dump-file ,cache-file com.gigamonkeys.foo::*text-output*))
	     (t (funcall ,fn ,@value-forms))))))))

(defun clear-template-cache ()
  (setf *template-functions* (make-hash-table :test #'equal))
  (setf *template-components* (make-hash-table :test #'equal)))

(defun get-template-function (vars language filename package)
  (let ((template-changed-date (template-changed-date filename))
	(cached (gethash filename *template-functions*)))
    (cdr
     (if (<= template-changed-date (or (car cached) 0))
	 cached
	 (setf (gethash filename *template-functions*) 
	       (cons template-changed-date (compile-template vars language filename package)))))))

(defun compile-template (vars language filename package)
  (let ((*template-file* filename))
    (setf (gethash *template-file* *template-components*) (list filename))
    (compile nil (template->lambda vars language filename package))))

(defun template->lambda (vars language filename package)
  `(lambda (,@vars)
     (declare (ignorable ,@vars))
     (let ((*package* ,package)
	   (*template-file* ,filename))
       (,language ,@(file->list filename package)))))

(defun normalize-bindings (vars)
  (mapcar #'(lambda (x) (etypecase x (symbol (list x x)) (cons x))) vars))


(defun template-changed-date (template-filename)
  (let ((components (gethash template-filename *template-components*)))
    (reduce #'max (mapcar #'file-write-date components) :initial-value 1)))

(defun add-template-component (file)
  "Add `file' as a component of the current template so that changes
to `file' will cause the template expansion function to be recompiled.
This is for use in FOO macros such interpolate the contens of other
files as FOO code into the template. There is no neeed to add files as
template components that dynamically effect the output of the template."
  (push file (gethash *template-file* *template-components*)))



