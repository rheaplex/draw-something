;; random.lisp - Choosing elements in various ways.
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
;; You should have received a copy of the GctNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random choice functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-init (seed)
  "Initialize the random number generator."
  (setf mt19937:*random-state*
        (mt19937::make-random-object
         :state (mt19937:init-random-state seed))))

(defun random-number (a)
  "The built-in random doesn't like 0.0 ."
  (if (= a 0)
      a
      (mt19937:random a)))
  
(defun random-range (a b)
  "Make a random number from a to below b."
  (let ((range (- b a)))
    (if (= range 0)
        a
        (+ (mt19937:random range) a))))

(defun random-range-inclusive (a b)
  "Make a random number from a to below b."
  (declare (type integer a b))
  (let ((range (+ (- b a) 1)))
    (if (= range 0)
        a
        (+ (mt19937:random range) a))))

(defgeneric choose-one-of (possibilities)
  (:documentation "Choose an item randomly from within the argument."))

(defmethod choose-one-of ((possibilities list))
  "Choose one or none of the options."
  (nth (mt19937:random (length possibilities)) possibilities))

(defmethod choose-one-of ((possibilities vector))
  "Choose one or none of the options."
  (aref possibilities (mt19937:random (length possibilities))))

(defun maybe-choose-one-of (possibilities)
  "Choose one or none of the options."
  (when (< (mt19937:random 1.0) 0.5)
    (choose-one-of possibilities)))

(defun maybe-choose-some-of (possibilities probability)
  "Choose none or more possibilities when random 1.0 < probability for it."
  (loop for item in possibilities
     when (< (mt19937:random 1.0) probability)
     collect item))

(defgeneric choose-n-of (n choices)
  (:documentation "Choose n items randomly from within the argument."))

(defmethod choose-n-of ((n integer) (choice-list list))
  "Choose n different entries from choice-list."
  (assert (<= n (length choice-list)))
  (let ((choices choice-list)
        (chosen '()))
    (dotimes (i n)
      (let ((choice (choose-one-of choices)))
        (setf chosen (cons choice chosen))
        (setf choices (remove choice choices))))
    chosen))

(defmethod choose-n-of ((n integer) (choice-vector vector))
  "Choose n different entries from choice-vector."
  (assert (<= n (length choice-vector)))
  (let ((choices choice-vector)
        (chosen (make-array n :fill-pointer 0)))
    (dotimes (i n)
      (let ((choice (choose-one-of choices)))
        (vector-push-extend choice chosen)
        (setf choices (remove choice choices))))
    chosen))

(defgeneric shuffle (source)
  (:documentation "Randomly re-order the items in the argument in-place."))

(defmethod shuffle ((l list))
  "Shuffle the list in place"
  (loop for i below (length l) do
    (rotatef
     (elt l i)
     (elt l (mt19937:random (length l)))))
  l)

(defmethod shuffle ((v vector))
  "Shuffle the vector in place"
  ;; Fisher-Yates shuffle
  (loop for n from (- (length v) 1) downto 0
       do (let* ((k (random-number (+ n 1))) ;; 0 <= k <= n
         (temp (aref v k)))
        ;; Simple variable swap
        (setf (aref v k) (aref v n))
        (setf (aref v n) temp)))
  v)

(defun choose-n-of-ordered (n choice-list)
  "Choose n of the entries, and ensure they are in order."
  ;; Not very efficient at all
  (let ((choices (choose-n-of n choice-list)))
    (loop for i in choice-list
           when (member i choices)
           collect i)))

(defun prefs-range (spec)
  "Get the total probability range of a prefs spec."
  (loop for prob in spec by #'cddr
        sum prob))

(defun prefs-cond (spec)
  "Make a cond to choose an option. eg (prefs 4 'a 4 'b 2 'c)"
  `(let ((i (mt19937:random ,(prefs-range spec))))
    (cond
      ,@(loop for prob in spec by #'cddr
              for val in (cdr spec) by #'cddr
              sum prob into prob-so-far
              collect `((< i ,prob-so-far) ,val)))))

(defmacro prefs (&rest spec)
  "Make a prefs cond to choose an option. eg (prefs 4 'a 4 'b 2 'c)"
  (prefs-cond spec))

(defmacro prefs-list (spec)
  "Make a prefs cond to choose an option. eg (prefs-list '(4 'a 3 'b))"
  (prefs-cond spec))

(defun prefs-lambda (&rest spec)
  "Make a lambda to choose an option. eg (prefs-lambda 4 'a 4 'b 2 'c)"
  (eval `(lambda () ,(prefs-cond spec))))

(defun prefs-list-lambda (spec)
  "Make a lambda to choose an option. eg (prefs-list-lambda '(4 'a 3 'b))"
  (eval `(lambda () ,(prefs-cond spec))))
