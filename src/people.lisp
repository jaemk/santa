(defpackage santa.people
  (:use :cl :arrow-macros :metabang-bind)
  (:export
    :*admin-numbers*
    :*people-numbers*
    :*forbidden-pairs*
    :*blurbs*
    ))
(in-package :santa.people)
(named-readtables:in-readtable :interpol-syntax)

(defparameter *admin-numbers*
  (list
    "+10001112222"))

(defparameter *people-numbers*
  (list
    (list "james"  "+10001112222")
    (list "lauren" "+10001112222")
    (list "sean"   "+10001112222")
    (list "jordan" "+10001112222")
    (list "ryan"   "+10001112222")
    (list "amanda" "+10001112222")))

(defparameter *forbidden-pairs*
  (list
    (list "james"  (list "lauren"))
    (list "lauren" (list "james"))
    (list "sean"   (list "jordan"))
    (list "jordan" (list "sean"))
    (list "ryan"   (list "amanda"))
    (list "amanda" (list "ryan"))))

(defparameter *blurbs*
  (list
    (list "james"  "James is cool")
    (list "lauren" "Lauren is cool")
    (list "sean"   "Sean is cool")
    (list "jordan" "Jordan is cool")
    (list "ryan"   "Ryan is cool")
    (list "amanda" "Amanda is cool")
    ))

(when (and (santa.config:value :load-local-people)
           (uiop:file-exists-p "local/people.lisp"))
  (load "local/people.lisp"))

