;; utilities.lisp - Various utilities.
;; Copyright (C) 2006, 2016, 2021 Rhea Myers.
;; Copyright (C) 2023 Myers Studio Ltd.
;;
;; This file is part of draw-something.
;;
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dequote (item)
  "Remove the quote from a symbol, returning the symbol."
  (cadr item))

(defmacro dotimesloop ((var from start to) &rest body)
  "Loop from start below to, then from from below to e.g. for 0 5 10
var = 5, 6, 7, 8, 9, 0, 1, 2, 3, 4
Note that this is evaluated as two loops"
  `(progn (loop for ,var from ,start below ,to
                do (progn ,@body))
          (loop for ,var from ,from below ,start
                do (progn ,@body))))

(defmacro make-hash (&rest key-values)
  (let ((hash (gensym))
        (key-value-list (gensym))
        (current-pair (gensym)))
    `(let ((,key-value-list ,key-values)
           (,hash (make-hash-table)))
    (dolist (,current-pair ,key-values)
      (setf (gethash (car ,current-pair)) (cdr ,current-pair)))
       ,hash)))

(defmethod make-vector (initial-size)
  "Make a stretchy vector."
  (make-array initial-size
              :adjustable t
              :fill-pointer 0))

(defmacro with-gensyms ((&rest names) &body body)
  "From Peter Siebel's Practical Common Lisp"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
