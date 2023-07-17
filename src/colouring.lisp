;; colouring.lisp - Creating and applying colour schemes.
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

(defun hs-combined (colour)
  (- (brightness colour)
     (saturation colour)))

(defun sort-colours-increasing-brightness (colours)
  (sort colours #'(lambda (a b) (< (brightness a)
                                   (brightness b)))))

(defun sort-colours-decreasing-brightness (colours)
  (sort colours #'(lambda (a b) (> (brightness a)
                                   (brightness b)))))

(defun sort-colours-increasing-saturation (colours)
  (sort colours #'(lambda (a b) (< (saturation a)
                                   (saturation b)))))

(defun sort-colours-decreasing-saturation (colours)
  (sort colours #'(lambda (a b) (> (saturation a)
                                   (saturation b)))))

(defun sort-colours-increasing-hue-and-saturation (colours)
  (sort colours #'(lambda (a b) (< (hs-combined a)
                                   (hs-combined b)))))

(defun sort-colours-decreasing-hue-and-saturation (colours)
  (sort colours #'(lambda (a b) (> (hs-combined a)
                                   (hs-combined b)))))

(defparameter +colour-plane-strategies+
  #(
    ;;sort-colours-increasing-brightness
    ;;sort-colours-decreasing-brightness
    ;;sort-colours-increasing-saturation
    ;;sort-colours-decreasing-saturation
    sort-colours-increasing-hue-and-saturation
    sort-colours-decreasing-hue-and-saturation
    ))

(defun make-buckets (bucket-count)
  (let ((buckets (make-array bucket-count)))
    (loop for i from 0 below bucket-count
          do (setf (aref buckets i)
                   (make-vector 1)))
    buckets))

(defun make-saturations-and-brightnesses (count)
  (loop for i from 0 to count
        collect (make-instance '<colour>
                               :saturation (random-range 0.2 1.0)
                               :brightness (random-range 0.5 0.95))))

(defun make-hues (count)
  (let ((hues (make-array count)))
    (loop for i from 0 below count
          do (setf (aref hues i) (random-number 1.0)))
    hues))

(defun sort-colours-to-buckets (colours strategy bucket-count)
  (let ((buckets (make-buckets bucket-count))
        (sorted (funcall strategy colours)))
    (loop for colour in sorted
          for i from 0 to (length sorted)
          do (vector-push-extend colour (aref buckets
                                              (round i bucket-count))))
    buckets))

(defun create-colours (bucket-count colour-count)
  (let* ((hues (make-hues bucket-count))
         (colours (make-saturations-and-brightnesses colour-count))
         (strategy (choose-one-of +colour-plane-strategies+))
         (buckets (sort-colours-to-buckets colours
                                           strategy
                                           bucket-count)))
    (format t "Colour strategy: ~a~%" strategy)
    (dotimes (i bucket-count)
      (loop for colour across (aref buckets i)
            do (setf (hue colour) (aref hues i))))
    buckets))

(defun choose-colour-for (colours bucket-index)
  (choose-one-of (aref colours bucket-index)))
