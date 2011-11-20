;;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;;
;;; See COPYING for details.

(defsystem monkeylib-text-languages
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :licence "BSD"
  :description "Compiler for text-based languages."
  :components
  ((:file "packages")
   (:file "language"       :depends-on ("packages"))
   (:file "file-compiler" :depends-on ("packages" "language")))
  :depends-on (:monkeylib-text-output
               :com.gigamonkeys.macro-utilities))
