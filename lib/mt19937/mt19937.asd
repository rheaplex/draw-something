;; -*- Lisp -*-

(defpackage #:mt19937-system
  (:use #:common-lisp #:asdf))

(in-package #:mt19937-system)

(defsystem mt19937
  :description "Portable MT19937 Mersenne Twister random number generator"
  :author "Douglas T. Crosher and Raymond Toy"
  :licence "Public domain"
  :version "1.1"
  :components ((:file "mt19937")))