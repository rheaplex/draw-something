;; plane.lisp - A plane (layer, level, plane) in the drawing.
;; Copyright (C) 2006, 2010, 2016 Rhea Myers.
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
;; A picture plane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <plane> ()
  ((figures :accessor figures
            :initform (make-array 1 :adjustable t :fill-pointer 0)
            :initarg :figures
            :documentation "The figures of the plane.")
   (pen-params :accessor pen-params
               :initarg :pen-params
               :documentation "The pen configuration for the plane."))
  (:documentation "A plane of the drawing."))

(defmethod print-object ((object <plane>) stream)
  "Make a human readable string describing the plane."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(FIGURES: ~a)" ;; PEN-PARAMS: ~a
            ;;(pen-params object)
            (figures object))))

(defun make-plane (&key pen-params)
  "Constuctor function."
  (make-instance '<plane> :pen-params pen-params))

(defun plane-forms-bounds (the-plane)
  "Get the bounding rectangles for every form of every figure on the plane"
  (let ((results (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for figure across (figures the-plane)
          do (loop for form across (forms figure)
                   do (vector-push-extend (bounds form) results)))
    results))

(defmacro do-plane-forms ((plane form-variable-name) &body body)
  "Run code for each form of each figure of a plane."
  (let ((figure-var (gensym)))
    `(loop for ,figure-var across (figures ,plane)
           do (loop for ,form-variable-name
                     across (forms ,figure-var)
                    do (progn ,@body)))))

(defparameter +find-point-tries+ 10000)
(defparameter +find-space-tries+ 1000)

(defun find-empty-point-on-plane (drawing plane)
  (let ((found nil))
    (loop for i from 0 below +find-point-tries+
          do (let ((point (random-point-in-rectangle (bounds drawing))))
             (when (not (intersects-any point (plane-forms-bounds plane)))
               (setf found point)
               (return))))
    found))

(defun find-space-on-plane-range-rects (drawing plane
                                        min-size-rect max-size-rect)
  "Find empty space on plane larger than min-size up to max-size, or nil"
  (let ((found nil)
        (exclude (plane-forms-bounds plane)))
    (loop for i from 0 below +find-space-tries+
          do (let ((start-point (find-empty-point-on-plane drawing plane))
                   (candidate nil))
               (when (not start-point)
                 (return))
               (setf candidate (grow-rectangle min-size-rect
                                               max-size-rect
                                               (x start-point)
                                               (y start-point)
                                               (bounds drawing)
                                               exclude))
               (when (not (not candidate))
                 (setf found (copy-rectangle candidate))
                 (return))))
    found))

(defun find-space-on-plane-range (drawing plane
                                  min-width min-height
                                  max-width max-height)
  "Find space on the plane, or nil if none can be found."
  (find-space-on-plane-range-rects drawing plane
                                   (make-rectangle :x 0
                                                   :y 0
                                                   :width min-width
                                                   :height min-height)
                                   (make-rectangle :x 0
                                                   :y 0
                                                   :width max-width
                                                   :height max-height)))

(defun find-space-on-plane (drawing plane size-rect)
  "Find empty space on the plane of given size, or nil if fail"
  (find-space-on-plane-range-rects drawing plane size-rect size-rect))

(defun plane-skeletons-points (plane)
  "Get a vector of every skeleton point on a plane"
  (let ((points (make-vector 1)))
    (do-plane-forms (plane f)
      (loop for s across (skeleton f)
            do (loop for p across (points s)
                     do (vector-push-extend points p))))))

(defun choose-plane-figure (plane)
  "Randomly choose a figure of the plane."
  (choose-one-of (figures plane)))
