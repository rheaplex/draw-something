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

;;FIXME: factor in hue
(defun ls-combined (colour)
  (+ (lightness colour)
     (- 1.0 (saturation colour))))

(defun sort-colours-increasing-lightness (colours)
  (sort colours #'(lambda (a b) (< (lightness a)
                                   (lightness b)))))

(defun sort-colours-decreasing-lightness (colours)
  (sort colours #'(lambda (a b) (> (lightness a)
                                   (lightness b)))))

(defun sort-colours-increasing-saturation (colours)
  (sort colours #'(lambda (a b) (< (saturation a)
                                   (saturation b)))))

(defun sort-colours-decreasing-saturation (colours)
  (sort colours #'(lambda (a b) (> (saturation a)
                                   (saturation b)))))

(defun sort-colours-increasing-lightness-and-saturation (colours)
  (sort colours #'(lambda (a b) (< (ls-combined a)
                                   (ls-combined b)))))

(defun sort-colours-decreasing-lightness-and-saturation (colours)
  (sort colours #'(lambda (a b) (> (ls-combined a)
                                   (ls-combined b)))))

(defparameter +colour-plane-strategies+
  #(;;sort-colours-increasing-lightness
    ;;sort-colours-decreasing-lightness
    ;;sort-colours-increasing-saturation
    ;;sort-colours-decreasing-saturation
    sort-colours-increasing-lightness-and-saturation
    sort-colours-decreasing-lightness-and-saturation))

(defun random-high ()
  (random-range .07 1.0))

(defun random-medium ()
  (random-range 0.4 0.6))

(defun low-range ()
  (random-range 0.1 0.3))

(defun increasing-gradient (a b steps)
  (let ((gradient (make-array steps))
        (step (/ (- b a) (- steps 1))))
    (loop for i from 0 below steps
          do (setf (aref gradient i)
                   (+ a (* step i))))
    gradient))

(defun decreasing-gradient (a b steps)
  (let ((gradient (make-array steps))
        (step (/ (- a b) (- steps 1))))
    (loop for i from 0 below steps
          do (setf (aref gradient i)
                   (- a (* step i))))
    gradient))

(defun make-hues (count)
  (let ((hues (make-array count)))
    (loop for i from 0 below count
          do (setf (aref hues i) (random-number 360.0)))
    hues))

(defun create-colours (count each-plane-count)
  (declare (ignore each-plane-count))
  (let ((saturations (if (> (random-number 1.0) 0.5)
                         (increasing-gradient (random-range 0.1 0.4)
                                              (random-range 0.6 0.9)
                                              count)
                         (decreasing-gradient (random-range 0.6 0.9)
                                              (random-range 0.1 0.4)
                                              count)))
        (lightnesses (if (> (random-number 1.0) 0.5)
                         (increasing-gradient (random-range 0.4 0.5)
                                              (random-range 0.6 0.9)
                                              count)
                         (decreasing-gradient (random-range 0.6 0.9)
                                              (random-range 0.4 0.5)
                                              count)))
        (hues (make-hues count))
        (colours (make-vector count)))
    (dotimes (i count)
      (setf (aref colours i)
            (make-colour :hue (aref hues i)
                         :saturation (aref saturations i)
                         :lightness (aref lightnesses i))))
    colours))

(defun choose-colour-for (colours bucket-index)
  (aref colours bucket-index))

;; (defun make-buckets (bucket-count)
;;   (let ((buckets (make-array bucket-count)))
;;     (loop for i from 0 below bucket-count
;;           do (setf (aref buckets i)
;;                    (make-vector 1)))
;;     buckets))

;; (defun make-saturations-and-lightnesses (count)
;;   (loop for i from 0 to count
;;         collect (make-instance '<colour>
;;                                :saturation (random-range 0 1.0)
;;                                :lightness (random-range 0 1.0))))

;; (defun sort-colours-to-buckets (colours strategy bucket-count)
;;   (let ((buckets (make-buckets bucket-count))
;;         (sorted (funcall strategy colours)))
;;     (loop for colour in sorted
;;           for i from 0 to (length sorted)
;;           do (vector-push-extend colour (aref buckets
;;                                               (round i bucket-count))))
;;     buckets))

;; (defun create-colours (bucket-count colour-count)
;;   (let* ((hues (make-hues bucket-count))
;;          (colours (make-saturations-and-lightnesses colour-count))
;;          (strategy (choose-one-of +colour-plane-strategies+))
;;          (buckets (sort-colours-to-buckets colours
;;                                            strategy
;;                                            bucket-count)))
;;     (format t "Colour strategy: ~a~%" strategy)
;;     (dotimes (i bucket-count)
;;       (loop for colour across (aref buckets i)
;;             do (setf (hue colour) (aref hues i))))
;;     buckets))

;; (defun choose-colour-for (colours bucket-index)
;;   (choose-one-of (aref colours bucket-index)))
