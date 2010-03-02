;;
;; Copyright (c) 2007, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.foo.templates
  :components
  ((:file "packages")
   (:file "templates" :depends-on ("packages")))
  :depends-on (:com.gigamonkeys.foo
	       :com.gigamonkeys.foo
	       :com.gigamonkeys.macro-utilities
	       :com.gigamonkeys.markup
	       :com.gigamonkeys.utilities))

