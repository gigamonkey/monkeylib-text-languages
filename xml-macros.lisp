;;
;; Copyright (c) 2005-2007, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.xml)

(define-xml-macro :? (name &rest attrs)
  `(:progn 
    (:noescape (:format "<?~(~a~) ~@{~(~a~)=\"~a\"~^ ~}?>" ,name ,@attrs))
    (:newline)))

(define-html-macro :? (name &rest attrs)
  `(:progn 
    (:noescape (:format "<?~a ~@{~(~a~)=\"~a\"~^ ~}?>" ,name ,@attrs))
    (:newline)))

(define-xml-macro :doctype (name type id url)
  `(:progn
    (:noescape (:format "<!DOCTYPE ~a ~a \"~a\" \"~a\">" ,name ,type ,id ,url))
    (:newline)))

(define-xml-macro :character (name)
  `(:noescape (:format ,(if (numberp name) "&#~d;" "&~(~a~);") ,name)))


