;;  utilities.lisp - Various utilities.
;;  Copyright (C) 2006, 2016, 2021 Rhea Myers
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

(in-package "DRAW-SOMETHING")

(defvar *print-advisories* t)
(defvar *print-debugs* t)

(defun debug-message (msg)
  "Write the message to the error stream, not to standard output."
  (when *print-debugs*
    (format *debug-io*
        "~A~%"
        msg)
    (finish-output *debug-io*)))

(defun advisory-message (msg)
  "Write the message to the error stream, not to standard output. No newline."
  (when *print-advisories*
    (format *debug-io*
        msg)
    (finish-output *debug-io*)))

(defun make-vector (initial-size)
  "Make a stretchy vector."
  (make-array initial-size
              :adjustable t
              :fill-pointer 0))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &rest body)
  `(do ()
       (,test)
     ,@body))

(defmacro dovector ((var vec) &rest body)
  `(loop for ,var across ,vec
      do (progn ,@body)))

(defmacro dotimesloop ((var from start to) &rest body)
  "Loop from start below to, then from from below to e.g. for 0 5 10
var = 5, 6, 7, 8, 9, 0, 1, 2, 3, 4
Note that this is evaluated as two loops"
  `(progn (loop for ,var from ,start below ,to
         do (progn ,@body))
      (loop for ,var from ,from below ,start
         do (progn ,@body))))

(defun dequote (item)
  "Remove the quote from a symbol, returning the symbol."
  (cadr item))

(defun random-init (seed)
  "Initialize the random number generator."
  (setf MT19937:*random-state*
        (MT19937::make-random-object :state (MT19937:init-random-state seed))))

(defun %random (below)
  "Generate a random number 0 <= n < below."
  (MT19937:random below))

(defun random-number (a)
  "The built-in random doesn't like 0.0 ."
  (if (= a 0)
      a
    (%random a)))

(defun random-range (a b)
  "Make a random number from a to below b."
  (let ((range (- b a)))
    (if (= range 0)
        a
        (+ (%random range) a))))

(defun random-range-inclusive (a b)
  "Make a random number from a to below b."
  (declare (type integer a b))
  (let ((range (+ (- b a) 1)))
    (if (= range 0)
        a
        (+ (%random range) a))))

(defgeneric choose-one-of (possibilities)
  (:documentation "Choose an item randomly from within the argument."))

(defmethod choose-one-of ((possibilities list))
  "Choose one or none of the options."
  (nth (%random (length possibilities)) possibilities))

(defmethod choose-one-of ((possibilities vector))
  "Choose one or none of the options."
  (aref possibilities (%random (length possibilities))))

(defun maybe-choose-one-of (possibilities)
  "Choose one or none of the options."
  (when (< (%random 1.0) 0.5)
    (choose-one-of possibilities)))

(defun maybe-choose-some-of (possibilities probability)
  "Choose none or more possibilities when random 1.0 < probability for it."
  (loop for item in possibilities
     when (< (%random 1.0) probability)
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
        (chosen (make-vector n)))
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
     (elt l (%random (length l)))))
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
  `(let ((i (%random ,(prefs-range spec))))
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

(defconstant +normal-to-255-multiplier+ (/ 1.0 256))

(defun normal-to-255 (normal)
  "Convert a 0..1 value to a 0..255 value."
  (* normal +normal-to-255-multiplier+))

(defmacro make-hash (&rest key-values)
  (let ((hash (gensym))
        (key-value-list (gensym))
        (current-pair (gensym)))
    `(let ((,key-value-list ,key-values)
           (,hash (make-hash-table)))
    (dolist (,current-pair ,key-values)
      (setf (gethash (car ,current-pair)) (cdr ,current-pair)))
    ,hash)))

(defmacro with-gensyms ((&rest names) &body body)
  "From Peter Siebel's Practical Common Lisp"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun closest-to-zero (a b)
  "Return value of a or b that is closest to zero."
  (if (< (abs a) (abs b))
      a
    b))

(defun hex-char-p (char)
  "Return whether the character is a (lowercase...) hex char"
  (if (find char "0123456789abcdef" :test #'eq)
      t
    nil))

(defun hex-string-p (str)
  "Return whether the string is a (lowercase...) hex string"
  (every #'hex-char-p str))

(defun split-seq-stride (seq stride)
  "Split a sequence (string, list, etc.) into a list of chunks
   of the given stride length (last chunk may be shorter)"
  (assert (> stride 0))
  (let ((len (length seq)))
    (loop for i from 0 below len by stride
          collect (subseq seq i (min len (+ i stride))))))

(defun hash-to-ints (hash)
  "Convert a 64-char hexadecimal hash value to a list of 32 ints from 0.255"
  (assert (= (length hash) 64))
  (assert (hex-string-p hash))
  (map 'list
       #'(lambda (n) (parse-integer n :radix 16))
       (split-seq-stride hash 2)))

;; These are here so ps/svg can access them.

(defvar *save-directory* "./")

(defun generate-filename (&optional (suffix ".eps"))
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil
            "~a~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A~a"
            *save-directory*
            "drawing" year month date hours minutes seconds suffix)))
