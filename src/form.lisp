;; form.lisp - A form within a figure.
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
;; A form in a figure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +min-form-points+ 1)
(defconstant +max-form-points+ 12)

(defconstant +form-step-limit+ 10000)

(defconstant +pen-width+ 1.0)

(defclass <form> ()
  ((skeleton :accessor skeleton
             :type vector
             :initarg :skeleton
             :initform (make-array 1 :adjustable t :fill-pointer 0)
             :documentation "The guide shapes for the outline.")
   (outline :accessor outline
            :type polyline
            :initform (make-polyline)
            :documentation "The outlines for the skeleton. Will be outline_s_.")
   (bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The bounds of the form.")
   (fill-colour :accessor fill-colour
                :type <colour>
                :initarg :colour
                :initform (make-colour :hue 0.0
                                       :saturation 1.0
                                       :brightness 1.0)
                :documentation "The flat body colour of the form.")
   (stroke-colour :accessor stroke-colour
                  :type <colour>
                  :initarg :colour
                  :initform (make-colour :hue 0.0
                                         :saturation 0.0
                                         :brightness 0.0)
                  :documentation "The outline colour of the form."))
  (:documentation "A form drawn in the drawing."))


(defun make-form (&key skeleton fill-colour stroke-colour)
  "constructor function"
  (make-instance '<form> :skeleton skeleton
                         :bounds (bounds skeleton)
                         :fill-colour fill-colour
                         :stroke-colour stroke-colour))

;; Skeleton will ultimately be generated from a list of objects, kept separately
;; Forms will be able to have no fill or no outline independently

(defun form-first-point (the-form)
  "Get the first point in the outline of the form."
  (first-point (outline the-form)))

(defun form-point-count (the-form)
  "The number of points in the outline of the form."
  (point-count (outline the-form)))

(defun most-recent-point (the-form)
  "The most recent point added to the outline of the form."
  (last-point (outline the-form)))

(defun make-form-start-point (form-skeleton pen-params)
  "Get the point to start drawing at."
  (let ((start-point nil))
    (loop for skel across form-skeleton
          do (let* ((hp (highest-leftmost-point skel))
                    (candidate (make-point :x (x hp)
                                           :y (+ (y hp)
                                                 (pen-distance pen-params)))))
               (when (or (not start-point)
                         (> (y candidate) (y start-point))
                         (and (= (y candidate) (y start-point))
                              (< (x candidate) (x start-point))))
                 (setf start-point candidate))))
    start-point))

(defun make-form-turtle (the-form pen-params)
  "Make the turtle to draw around the form."
  (make-turtle :location (make-form-start-point (skeleton the-form)
                                                pen-params)
               :direction (- (/ pi 2.0))))

#|(defun make-form-from-points (points)
  "Make a form, ready to be started."
  (log-info "Making form points.")
  (let* ((skel (make-polyline-from-points points))
         (the-form (make-form :skeleton (vector skel)
                              :bounds (bounds skel)))
         (draw-form the-form)
the-form)))|#

(defun path-ready-to-close (the-form pen-params)
  (and (> (form-point-count the-form) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-form)
                    (form-first-point the-form))
          (move-step pen-params))))

(defun path-timeout (the-form)
  "Make sure that a failure of the form algorithm hasn't resulted in a loop."
  (let ((has-timed-out (> (form-point-count the-form)
                          +form-step-limit+)))
    (when has-timed-out
      (log-err "ERROR: FORM PATH TIMED OUT ============================"))
    has-timed-out))

(defun should-finish-form (the-form pen-params)
  "Decide whether the form should finish."
  (or (path-ready-to-close the-form pen-params)
      (path-timeout the-form)))

(defun draw-form (the-form pen-params)
  "Find the next point forward along the drawn outline of the shape."
  (let* ((form-bounds (bounds the-form))
         (the-outline (outline the-form))
         (the-turtle (make-form-turtle the-form pen-params)))
    (log-info "Drawing form.")
    (append-point the-outline (location the-turtle))
    (loop until (should-finish-form the-form pen-params)
          do (progn
               (adjust-next-pen (skeleton the-form) pen-params the-turtle)
               (forward the-turtle (move-step pen-params))
               (let ((new-location (location the-turtle)))
                 (append-point the-outline new-location)
                 (include-point form-bounds new-location))))
    (append-point the-outline (form-first-point the-form))))
